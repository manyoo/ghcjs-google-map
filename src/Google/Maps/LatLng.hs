{-# LANGUAGE JavaScriptFFI #-}
module Google.Maps.LatLng where

import GHCJS.Types

type LatLng = JSVal

type Lat = Double
type Lng = Double

foreign import javascript unsafe "new google.maps.LatLng($1, $2)"
    mkLatLng :: Lat -> Lng -> LatLng

foreign import javascript unsafe "($2).equals($1)"
    equals :: LatLng -> LatLng -> Bool

foreign import javascript unsafe "($1).lat()"
    lat :: LatLng -> Lat

foreign import javascript unsafe "($1).lng()"
    lng :: LatLng -> Lng
