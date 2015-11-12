{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Google.Maps.Map (
    GMap, mkMap, getCenter, getDiv, getHeading, getMapType, setMapType, getTilt,
    getZoom, panBy, panTo, setCenter, setOptions, setTilt, setZoom, MapOptionItem(..),
    MapOption
    ) where

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Node
import Data.JSString
import JavaScript.Object

import Control.Monad

import Google.Maps.Types
import Google.Maps.LatLng

type GMap = JSVal

foreign import javascript unsafe "new google.maps.Map($1, $2)"
    jsMkMap :: Node -> JSMapOptions -> IO GMap

mkMap :: Node -> MapOption -> IO GMap
mkMap n opt = toJSOption opt >>= jsMkMap n

foreign import javascript unsafe "($1).getCenter()"
    getCenter :: GMap -> IO LatLng

foreign import javascript unsafe "($1).getDiv()"
    getDiv :: GMap -> IO Node

foreign import javascript unsafe "($1).getHeading()"
    getHeading :: GMap -> IO Heading

foreign import javascript unsafe "($1).getMapTypeId()"
    getMapType :: GMap -> IO MapType

foreign import javascript unsafe "($2).setMapTypeId($1)"
    setMapType :: MapType -> GMap -> IO ()

foreign import javascript unsafe "($1).getTilt()"
    getTilt :: GMap -> IO Tilt

foreign import javascript unsafe "($1).getZoom()"
    getZoom :: GMap -> IO ZoomLevel

foreign import javascript unsafe "($3).panBy($1, $2)"
    panBy :: Pixel -> Pixel -> GMap -> IO ()

foreign import javascript unsafe "($2).panTo($1)"
    panTo :: LatLng -> GMap -> IO ()

foreign import javascript unsafe "($2).setCenter($1)"
    setCenter :: LatLng -> GMap -> IO ()

foreign import javascript unsafe "($2).setOptions($1)"
    jsSetOptions :: JSMapOptions -> GMap -> IO ()

setOptions :: MapOption -> GMap -> IO ()
setOptions opt m = toJSOption opt >>= flip jsSetOptions m

foreign import javascript unsafe "($2).setTilt($1)"
    setTilt :: Tilt -> GMap -> IO ()

foreign import javascript unsafe "($2).setZoom($1)"
    setZoom :: ZoomLevel -> GMap -> IO ()

data MapOptionItem = OptBackGroundColor JSString
                   | OptCenter LatLng
                   | OptDisableDefaultUI Bool
                   | OptDisableDoubleClickZoom Bool
                   | OptDraggable Bool
                   | OptDraggableCursor JSString
                   | OptDraggingCursor JSString
                   | OptHeading Heading
                   | OptKeyboardShortcuts Bool
                   | OptMapMaker Bool
                   | OptMapTypeControl Bool
                   | OptMapType MapType
                   | OptMaxZoom ZoomLevel
                   | OptMinZoom ZoomLevel
                   | OptNoClear Bool
                   | OptOverviewMapControl Bool
                   | OptPanControl Bool
                   | OptRotateControl Bool
                   | OptScaleControl Bool
                   | OptScrollWheel Bool
                   | OptStreetViewControl Bool
                   | OptTilt Tilt
                   | OptZoom ZoomLevel
                   | OptZoomControl Bool

type MapOption = [MapOptionItem]

toJSVals :: MapOptionItem -> IO (JSString, JSVal)
toJSVals (OptBackGroundColor c) = toJSValsHelper "backgroundColor" c
toJSVals (OptCenter c) = toJSValsHelper "center" c
toJSVals (OptDisableDefaultUI d) = toJSValsHelper "disableDefaultUI" d
toJSVals (OptDisableDoubleClickZoom d) = toJSValsHelper "disableDoubleClickZoom" d
toJSVals (OptDraggable d) = toJSValsHelper "draggable" d
toJSVals (OptDraggableCursor c) = toJSValsHelper "draggableCursor" c
toJSVals (OptDraggingCursor c) = toJSValsHelper "draggingCursor" c
toJSVals (OptHeading h) = toJSValsHelper "heading" h
toJSVals (OptKeyboardShortcuts k) = toJSValsHelper "keyboardShortcuts" k
toJSVals (OptMapMaker m) = toJSValsHelper "mapMaker" m
toJSVals (OptMapTypeControl m) = toJSValsHelper "mapTypeControl" m
toJSVals (OptMapType t) = toJSValsHelper "mapTypeId" t
toJSVals (OptMaxZoom z) = toJSValsHelper "maxZoom" z
toJSVals (OptMinZoom z) = toJSValsHelper "minZoom" z
toJSVals (OptNoClear n) = toJSValsHelper "noClear" n
toJSVals (OptOverviewMapControl o) = toJSValsHelper "overviewMapControl" o
toJSVals (OptPanControl p) = toJSValsHelper "panControl" p
toJSVals (OptRotateControl p) = toJSValsHelper "rotateControl" p
toJSVals (OptScaleControl s) = toJSValsHelper "scaleControl" s
toJSVals (OptScrollWheel s) = toJSValsHelper "scrollWheel" s
toJSVals (OptStreetViewControl s) = toJSValsHelper "streetViewControl" s
toJSVals (OptTilt t) = toJSValsHelper "tilt" t
toJSVals (OptZoom z) = toJSValsHelper "zoom" z
toJSVals (OptZoomControl c) = toJSValsHelper "zoomControl" c

toJSOption :: MapOption -> IO JSMapOptions
toJSOption opts = do
    obj <- create
    forM_ opts (\item -> do
        (k, v) <- toJSVals item
        setProp k v obj)
    return obj
