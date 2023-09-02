module Routes (routes) where

import Prelude ((.), ($), pure)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, responseLBS)
import Models.Config (Config)

routes ∷ (MonadIO m, MonadReader Config m) ⇒ Request → m Response
routes α = case (pathInfo α) of
  (_                    ) → pure $ textResponse status404 "Invalid path."

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


--route ∷ MonadIO m ⇒ Request → TVar Cmus → Resources → m Response
--route α ω r = case (pathInfo α, requestMethod α) of
--  ("add"      : ns  : [], "GET") → addTrack ω ns
--  ("remove"   : n:m : [], "GET") → removeTrack ω n m
--  ("sync"           : [], "GET") → fullSync ω
--  ("queue"          : [], "GET") → queue ω
--  ("style.css"      : [], "GET") → pure $ cssResponse status200 $ css r
--  ("index.js"       : [], "GET") → pure $ jsResponse status200 $ js r
--  ("play"           : [], "GET") → play $> textResponse status200 "Toggled."
--  ("vol"      : n   : [], "GET") → setVolume n
--  ([]                   , "GET") → pure $ htmlResponse status200 $ html r
--  (_                    , _    ) → pure $ textResponse status404 "Invalid path."
--
--addTrack ∷ MonadIO m ⇒ TVar Cmus → Text → m Response
--addTrack α n = handle "Error adding tracks." $ add α n
--
--removeTrack ∷ MonadIO m ⇒ TVar Cmus → Text → Text → m Response
--removeTrack α n m = handle "Error removing tracks." $ remove α n m
--
--fullSync ∷ MonadIO m ⇒ TVar Cmus → m Response
--fullSync α = handle "Error syncing library." $ sync α
--
--setVolume ∷ MonadIO m ⇒ Text → m Response
--setVolume n = handle "Error setting volume." $ volume n
--
--queue ∷ MonadIO m ⇒ TVar Cmus → m Response
--queue α = handle "Error getting queue." $ getQueue α
--
--handle ∷ (MonadIO m, ToJSON a) ⇒ Text → ExceptT Status m a → m Response
--handle α ω = respond <$> (runExceptT ω)
--  where respond = flip textResponse α ||| jsonResponse status200
