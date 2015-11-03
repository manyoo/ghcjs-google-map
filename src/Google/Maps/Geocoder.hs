{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Google.Maps.Geocoder where

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
type GeocoderResult = Object

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
        res <- fmap (fmap Object) <$> fromJSVal jsVal1
        sts <- fromJSVal jsVal2
        if isJust res && isJust sts
            then cb (fromJust res) (fromJust sts)
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
