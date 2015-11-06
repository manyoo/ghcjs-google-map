{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Google.Maps.Autocomplete (
    AutoComplete, PlacesServiceStatus, AutoCompleteRequestItem(..), AutoCompleteRequest,
    AutoCompletePrediction(..), PredictionTerm(..), PredictionSubstring(..),
    mkAutoComplete, getPlacePredictions, psInvalidRequest, psOK, psOverQueryLimit,
    psRequestDenied, psUnknownError, psZeroResults
    ) where

import GHCJS.Types
import GHCJS.Marshal
import Data.JSString
import JavaScript.Object.Internal

import Control.Monad
import Data.Maybe (fromJust, isJust)

import Google.Maps.Types
import Google.Maps.LatLng

type AutoComplete = JSVal
type JSAutoCompleteRequest = Object
type JSAutoCompletePrediction = Object

type PlacesServiceStatus = JSString

-- | create a new AutoCompleteService object
foreign import javascript unsafe "new google.maps.places.AutoCompleteService()"
    mkAutoComplete :: IO AutoComplete

foreign import javascript interruptible "($2).getPlacePredictions($1, function(acpArray, status) { $c({predictions: acpArray, status: status}); });"
    jsGetPlacePredictions :: JSAutoCompleteRequest -> AutoComplete -> IO JSVal

getPlacePredictions :: AutoCompleteRequest -> AutoComplete -> IO (Maybe ([AutoCompletePrediction], PlacesServiceStatus))
getPlacePredictions req ac = do
    jsreq <- toJSAutoCompleteRequest req
    resObj <- Object <$> jsGetPlacePredictions jsreq ac
    -- convert JSVal to Maybe [AutoCompletePrediction]
    predList <- getProp "predictions" resObj
    jsPreds <- fmap (fmap Object) <$> fromJSVal predList
    sts <- getJSVal "status" resObj
    if isJust jsPreds && isJust sts
        then do -- convert [JSAutoCompletePrediction] to Maybe [AutoCompletePrediction]
            let jsPredList = fromJust jsPreds
            mayPredList <- mapM fromJSAutoCompletePrediction jsPredList >>= return . sequence
            if isJust mayPredList
                then return $ Just (fromJust mayPredList, fromJust sts)
                else return Nothing
        else return Nothing

data AutoCompleteRequestItem = ACRInput JSString  -- user input address
                             | ACRLocation LatLng
                             | ACROffset Int
                             | ACRRadius Double
                             | ACRTypes [JSString]

type AutoCompleteRequest = [AutoCompleteRequestItem]

toJSValTuple :: AutoCompleteRequestItem -> IO (JSString, JSVal)
toJSValTuple (ACRInput i) = toJSValsHelper "input" i
toJSValTuple (ACRLocation l) = toJSValsHelper "location" l
toJSValTuple (ACROffset o) = toJSValsHelper "offset" o
toJSValTuple (ACRRadius r) = toJSValsHelper "radius" r
toJSValTuple (ACRTypes t) = toJSValsHelper "types" t

toJSAutoCompleteRequest :: AutoCompleteRequest -> IO JSAutoCompleteRequest
toJSAutoCompleteRequest reqs = do
    obj <- create
    forM_ reqs (\req -> do
        (k, v) <- toJSValTuple req
        setProp k v obj
        )
    return obj

-- PlacesServiceStatus values imported from JS
foreign import javascript unsafe "google.maps.places.PlacesServiceStatus.INVALID_REQUEST"
    psInvalidRequest :: PlacesServiceStatus

foreign import javascript unsafe "google.maps.places.PlacesServiceStatus.OK"
    psOK :: PlacesServiceStatus

foreign import javascript unsafe "google.maps.places.PlacesServiceStatus.OVER_QUERY_LIMIT"
    psOverQueryLimit :: PlacesServiceStatus

foreign import javascript unsafe "google.maps.places.PlacesServiceStatus.REQUEST_DENIED"
    psRequestDenied :: PlacesServiceStatus

foreign import javascript unsafe "google.maps.places.PlacesServiceStatus.UNKNOWN_ERROR"
    psUnknownError :: PlacesServiceStatus

foreign import javascript unsafe "google.maps.places.PlacesServiceStatus.ZERO_RESULTS"
    psZeroResults :: PlacesServiceStatus

data PredictionTerm = PredictionTerm {
    ptOffset :: Double,
    ptValue :: JSString
}

data PredictionSubstring = PredictionSubstring {
    psLength :: Int,
    psOffset :: Double
}

data AutoCompletePrediction = AutoCompletePrediction {
    description :: JSString,
    matchedSubstrings :: [PredictionSubstring],
    placeId :: JSString,
    terms :: [PredictionTerm],
    types :: [JSString]
}

getJSVal n o = getProp n o >>= fromJSVal

fromJSPredictionTerm :: JSVal -> IO (Maybe PredictionTerm)
fromJSPredictionTerm v = do
    let obj = Object v
    os <- getJSVal "offset" obj
    v <- getJSVal "value" obj
    return $ PredictionTerm <$> os <*> v

fromJSPredictionSubstring :: JSVal -> IO (Maybe PredictionSubstring)
fromJSPredictionSubstring v = do
    let obj = Object v
    l <- getJSVal "length" obj
    os <- getJSVal "offset" obj
    return $ PredictionSubstring <$> l <*> os

fromJSAutoCompletePrediction :: JSAutoCompletePrediction -> IO (Maybe AutoCompletePrediction)
fromJSAutoCompletePrediction pred = do
    desc <- getJSVal "description" pred
    pId <- getJSVal "palce_id" pred
    tps <- getJSVal "types" pred

    mjssubstrs <- getJSVal "matched_substrings" pred
    mSubStrs <- case mjssubstrs of
        Nothing -> return Nothing
        Just jssubstrs -> mapM fromJSPredictionSubstring jssubstrs >>= return . sequence

    mjsterms <- getJSVal "terms" pred
    mTerms <- case mjsterms of
        Nothing -> return Nothing
        Just jsterms -> mapM fromJSPredictionTerm jsterms >>= return . sequence

    return $ AutoCompletePrediction <$> desc <*> mSubStrs <*> pId <*> mTerms <*> tps
