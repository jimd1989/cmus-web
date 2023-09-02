module Routes (routes) where

import Prelude ((.), ($), (<$>), flip, pure)
import Control.Arrow ((|||))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, responseLBS)
import Models.Config (Config)
import Repositories.Cmus (add, getQueue, play, remove, sync, volume)
import Repositories.Resources (staticCss, staticHtml, staticJs)

routes ∷ (MonadReader Config m, MonadIO m) ⇒ Request → m Response
routes α = case (pathInfo α) of
  ("add"       : ns     : []) → addTrack ns
  ("play"               : []) → togglePlay
  ("queue"              : []) → queue
  ("remove"    : n  : m : []) → removeTrack n m
  ("sync"               : []) → fullSync
  ("vol"       : n      : []) → setVolume n
  ("style.css"          : []) → cssResponse status200 <$> staticCss
  ("index.js"           : []) → jsResponse status200 <$> staticJs
  (                       []) → htmlResponse status200 <$> staticHtml
  (_                        ) → pure $ textResponse status404 "Invalid path."

handle ∷ (MonadIO m, ToJSON a) ⇒ Text → ExceptT Status m a → m Response
handle α ω = respond <$> (runExceptT ω)
  where respond = flip textResponse α ||| jsonResponse status200

addTrack ∷ (MonadReader Config m, MonadIO m) ⇒ Text → m Response
addTrack n = handle "Error adding tracks" $ add n

fullSync ∷ (MonadReader Config m, MonadIO m) ⇒ m Response
fullSync = handle "Error syncing library" $ sync

removeTrack ∷ (MonadReader Config m, MonadIO m) ⇒ Text → Text → m Response
removeTrack n m = handle "Error removing tracks" $ remove n m

queue ∷ (MonadReader Config m, MonadIO m) ⇒ m Response
queue = handle "Error getting queue" $ getQueue

setVolume ∷ MonadIO m ⇒ Text → m Response
setVolume n = handle "Error setting volume" $ volume n

togglePlay ∷ MonadIO m ⇒ m Response
togglePlay = play $> textResponse status200 "Toggled"

response ∷ ByteString → Status → Text → Response
response h α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, h)]
        convert = fromChunks . pure . encodeUtf8

cssResponse ∷ Status → Text → Response
cssResponse = response "text/css"

htmlResponse ∷ Status → Text → Response
htmlResponse = response "text/html"

jsResponse ∷ Status → Text → Response
jsResponse = response "text/javascript"

jsonResponse ∷ ToJSON a ⇒ Status → a → Response
jsonResponse α ω = responseLBS α headers (encode ω)
  where headers = [(hContentType, "application/json")]

textResponse ∷ Status → Text → Response
textResponse = response "text/plain"
