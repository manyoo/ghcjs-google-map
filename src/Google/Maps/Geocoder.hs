{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Google.Maps.Geocoder (
    Geocoder, GeocoderStatus, GeocodeRequestItem(..), GeocoderRequest, GeocoderResult(..),
    GeocoderAddressComponent(..), mkGeocoder, geocode, gsError, gsInvalidRequest,
    gsOK, gsOverQueryLimit, gsRequestDenied, gsUnknownError, gsZeroResults
    ) where

import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object.Internal

import Control.Monad
import Data.Maybe (fromJust, isJust)

import Google.Maps.Types
import Google.Maps.LatLng

type Geocoder = JSVal
type JSGeocoderRequest = Object
type JSGeocoderResult = Object

type GeocoderStatus = JSString

-- | create a new JS Geocoder object
foreign import javascript unsafe "new google.maps.Geocoder()"
    mkGeocoder :: IO Geocoder

-- wrapper for the actual JS geocode function
foreign import javascript unsafe "($3).geocode($1, $2)"
    jsGeocode :: JSGeocoderRequest -> Callback (JSVal -> JSVal -> IO ()) -> Geocoder -> IO ()

-- | a simplified API for geocode
geocode :: GeocoderRequest -> ([GeocoderResult] -> GeocoderStatus -> IO ()) -> Geocoder -> IO ()
geocode req cb geocoder = do
    jsCb <- syncCallback2 ThrowWouldBlock (\jsVal1 jsVal2 -> do
        -- convert JSVal to Maybe [JSGeocoderResult]
        jsres <- fmap (fmap Object) <$> fromJSVal jsVal1
        sts <- fromJSVal jsVal2
        if isJust jsres && isJust sts
            then do -- convert [JSGeocoderResult] to Maybe [GeocoderResult]
                let jsresList = fromJust jsres
                mayResList <- mapM fromJSGeocoderResult jsresList >>= return . sequence
                if isJust mayResList
                    then cb (fromJust mayResList) (fromJust sts)
                    else return ()
            else return ()
        )
    jsreq <- toJSGeocodeRequest req
    jsGeocode jsreq jsCb geocoder

-- GeocoderRequest data definition (not all fields supported yet)
data GeocodeRequestItem = GRAddress JSString
                        | GRLocation LatLng
                        | GRPlaceID JSString
                        | GRRegion JSString

type GeocoderRequest = [GeocodeRequestItem]

toJSValTuple :: GeocodeRequestItem -> IO (JSString, JSVal)
toJSValTuple (GRAddress a) = toJSValsHelper "address" a
toJSValTuple (GRLocation l) = toJSValsHelper "location" l
toJSValTuple (GRPlaceID p) = toJSValsHelper "placeId" p
toJSValTuple (GRRegion r) = toJSValsHelper "region" r

toJSGeocodeRequest :: GeocoderRequest -> IO JSGeocoderRequest
toJSGeocodeRequest reqs = do
    obj <- create
    forM_ reqs (\req -> do
        (k, v) <- toJSValTuple req
        setProp k v obj
        )
    return obj

-- GeocoderStatus values imported from JS
foreign import javascript unsafe "google.maps.GeocoderStatus.ERROR"
    gsError :: GeocoderStatus

foreign import javascript unsafe "google.maps.GeocoderStatus.INVALID_REQUEST"
    gsInvalidRequest :: GeocoderStatus

foreign import javascript unsafe "google.maps.GeocoderStatus.OK"
    gsOK :: GeocoderStatus

foreign import javascript unsafe "google.maps.GeocoderStatus.OVER_QUERY_LIMIT"
    gsOverQueryLimit :: GeocoderStatus

foreign import javascript unsafe "google.maps.GeocoderStatus.REQUEST_DENIED"
    gsRequestDenied :: GeocoderStatus

foreign import javascript unsafe "google.maps.GeocoderStatus.UNKNOWN_ERROR"
    gsUnknownError :: GeocoderStatus

foreign import javascript unsafe "google.maps.GeocoderStatus.ZERO_RESULTS"
    gsZeroResults :: GeocoderStatus


-- GeocoderResult from JSVals
data GeocoderAddressComponent = GeocoderAddressComponent {
    longName :: JSString,
    shortName :: JSString,
    gacTypes :: [JSString]
}

type JSGeocoderAddressComponent = Object

fromJSGeocoderAddressComponent :: JSGeocoderAddressComponent -> IO (Maybe GeocoderAddressComponent)
fromJSGeocoderAddressComponent comp = do
    let getJSVal n = getProp n comp >>= fromJSVal
    ln <- getJSVal "long_name"
    sn <- getJSVal "short_name"
    tps <- getJSVal "types"
    return $ GeocoderAddressComponent <$> ln <*> sn <*> tps

data GeocoderResult = GeocoderResult {
    addrComponents :: [GeocoderAddressComponent],
    formattedAddress :: JSString,
    partialMatch :: Bool,
    placeId :: JSString,
    postCodeLocalities :: [JSString],
    grTypes :: [JSString]
}

fromJSGeocoderResult :: JSGeocoderResult -> IO (Maybe GeocoderResult)
fromJSGeocoderResult res = do
    let getJSVal n = getProp n res >>= fromJSVal
    -- get the address_components JSVal first
    jsvalComp <- getProp "address_components" res
    -- convert JSVal to Maybe [JSGeocoderAddressComponent]
    jscomps <- fmap (fmap Object) <$> fromJSVal jsvalComp
    -- convert jscomps to Maybe [GeocoderAddressComponent]
    comps <- case jscomps of
                Nothing -> return Nothing
                Just jscompList -> mapM fromJSGeocoderAddressComponent jscompList >>= return . sequence
    faddr <- getJSVal "formatted_address"
    partMatch <- getJSVal "partial_match"
    pId <- getJSVal "place_id"
    pl <- getJSVal "postcode_localities"
    tps <- getJSVal "types"

    return $ GeocoderResult <$> comps <*> faddr <*> partMatch <*> pId <*> pl <*> tps
