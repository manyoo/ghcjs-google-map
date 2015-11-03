{-# LANGUAGE JavaScriptFFI #-}
module Google.Maps.Types where

import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Object

import Google.Maps.LatLng

type MapType = JSString

foreign import javascript unsafe "google.maps.MapTypeId.HYBRID"
    mapTypeHybrid :: MapType

foreign import javascript unsafe "google.maps.MapTypeId.ROADMAP"
    mapTypeRoadMap :: MapType

foreign import javascript unsafe "google.maps.MapTypeId.SATELLITE"
    mapTypeSatellite :: MapType

foreign import javascript unsafe "google.maps.MapTypeId.TERRAIN"
    mapTypeTerran :: MapType

type Tilt = Int
type ZoomLevel = Int
type Heading = Int
type Pixel = Int
type JSMapOptions = Object

-- | helper function to construct option values
c ~: v = c v

-- | a helper function used in this library for building JS objects easier
toJSValsHelper :: (ToJSVal v) => k -> v -> IO (k, JSVal)
toJSValsHelper k v = toJSVal v >>= return . (,) k
