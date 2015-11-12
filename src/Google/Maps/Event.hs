{-# LANGUAGE JavaScriptFFI #-}
module Google.Maps.Event where

import GHCJS.Types
import GHCJS.Foreign.Callback
import Data.JSString

import Google.Maps.Types
import Google.Maps.Map

type MapsEventListener = JSVal

foreign import javascript unsafe "google.maps.event.addListener($1, $2, $3, $4)"
    jsAddListener :: GMap -> JSString -> Callback (IO ()) -> Bool -> IO MapsEventListener

addListener :: GMap -> JSString -> IO () -> Bool -> IO MapsEventListener
addListener map evtName cb capture = do
    jsCb <- syncCallback ThrowWouldBlock cb
    jsAddListener map evtName jsCb capture

foreign import javascript unsafe "google.maps.event.removeListener($1)"
    removeListener :: MapsEventListener -> IO ()

foreign import javascript unsafe "google.maps.event.clearInstanceListeners($1)"
    clearInstanceListeners :: GMap -> IO ()

foreign import javascript unsafe "google.maps.event.clearListeners($1)"
    clearListeners :: GMap -> IO ()

foreign import javascript unsafe "google.maps.event.trigger($2, $1)"
    trigger :: JSString -> GMap -> IO ()
