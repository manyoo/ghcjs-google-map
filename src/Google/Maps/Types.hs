{-# LANGUAGE JavaScriptFFI #-}
module Google.Maps.Types where

import GHCJS.Types
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
