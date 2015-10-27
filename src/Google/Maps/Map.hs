{-# LANGUAGE JavaScriptFFI #-}
module Google.Maps.Map where

import GHCJS.Types
import GHCJS.DOM.Node

import Google.Maps.Types
import Google.Maps.LatLng

type Map = JSVal

foreign import javascript unsafe "new google.maps.Map($1, $2)"
    mkMap :: Node -> MapOptions -> IO Map

foreign import javascript unsafe "($1).getCenter()"
    getCenter :: Map -> IO LatLng

foreign import javascript unsafe "($1).getDiv()"
    getDiv :: Map -> IO Node

foreign import javascript unsafe "($1).getHeading()"
    getHeading :: Map -> IO Heading

foreign import javascript unsafe "($1).getMapTypeId()"
    getMapTypeId :: Map -> IO MapType

foreign import javascript unsafe "($1).getTilt()"
    getTilt :: Map -> IO Tilt

foreign import javascript unsafe "($1).getZoom()"
    getZoom :: Map -> IO ZoomLevel

foreign import javascript unsafe "($3).panBy($1, $2)"
    panBy :: Pixel -> Pixel -> Map -> IO ()

foreign import javascript unsafe "($2).panTo($1)"
    panTo :: LatLng -> Map -> IO ()

foreign import javascript unsafe "($2).setCenter($1)"
    setCenter :: LatLng -> Map -> IO ()

foreign import javascript unsafe "($2).setOptions($1)"
    setOptions :: MapOptions -> Map -> IO ()

foreign import javascript unsafe "($2).setTilt($1)"
    setTilt :: Tilt -> Map -> IO ()

foreign import javascript unsafe "($2).setZoom($1)"
    setZoom :: ZoomLevel -> Map -> IO ()
