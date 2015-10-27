{-# LANGUAGE JavaScriptFFI #-}
module Google.Maps.Geocoder where

import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object.Internal

import Data.Maybe (fromJust, isJust)

type Geocoder = JSVal
type GeocoderRequest = Object
type GeocoderResult = Object

type GeocoderStatus = JSString


foreign import javascript unsafe "new google.maps.Geocoder()"
    mkGeocoder :: IO Geocoder

foreign import javascript unsafe "($3).geocode($1, $2)"
    jsGeocode :: GeocoderRequest -> Callback (JSVal -> JSVal -> IO ()) -> Geocoder -> IO ()

geocode :: GeocoderRequest -> ([GeocoderResult] -> GeocoderStatus -> IO ()) -> Geocoder -> IO ()
geocode req cb geocoder = do
    jsCb <- syncCallback2 ThrowWouldBlock (\jsVal1 jsVal2 -> do
        res <- fmap (fmap Object) <$> fromJSVal jsVal1
        sts <- fromJSVal jsVal2
        if isJust res && isJust sts
            then cb (fromJust res) (fromJust sts)
            else return ()
        )
    jsGeocode req jsCb geocoder

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
