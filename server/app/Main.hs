module Main (main) where

import Prelude (Int, IO, (.), ($), (>>=), (<$>), const, flip, id, pure)
import Control.Arrow ((|||))
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipSettings(..), GzipFiles(..), def, gzip)
import System.Environment (getArgs)
import CmusRepository (add, getQueue, play, remove, sync, volume)
import Models (Cmus, cmus)
import Resources (Resources(..), resources)
import Helpers ((◀), (◁), head', readInt)

main ∷ IO ()
main = do
  port ← getPortNumber
  res  ← resources
  cms  ← newTVarIO $ cmus
  run port $ gzip gzipSettings $ \req send → route req cms res >>= send

gzipSettings ∷ GzipSettings
gzipSettings = def { gzipFiles = GzipCompress }

getPortNumber ∷ IO Int
getPortNumber = (const 1917 ||| id) . (readInt ◀ pack ◁ head') <$> getArgs

route ∷ Request → TVar Cmus → Resources → IO Response
route α ω r = case (pathInfo α, requestMethod α) of
  ("add"      : ns : [], "GET") → addTrack ω ns
  ("remove"   : n  : [], "GET") → removeTrack ω n
  ("sync"     :      [], "GET") → fullSync ω
  ("queue"    :      [], "GET") → queue ω
  ("index.js" :      [], "GET") → pure $ jsResponse status200 $ js r
  ("play"     :      [], "GET") → play $> textResponse status200 "Toggled."
  ("vol"      : n  : [], "GET") → setVolume n
  ([]                  , "GET") → pure $ htmlResponse status200 $ html r
  (_                   , _    ) → pure $ textResponse status404 "Invalid path."

addTrack ∷ TVar Cmus → Text → IO Response
addTrack α n = handle "Error adding tracks." $ add α n

removeTrack ∷ TVar Cmus → Text → IO Response
removeTrack α n = handle "Error removing tracks." $ remove α n

fullSync ∷ TVar Cmus → IO Response
fullSync α = handle "Error syncing library." $ sync α

setVolume ∷ Text → IO Response
setVolume n = handle "Error setting volume." $ volume n

queue ∷ TVar Cmus → IO Response
queue α = handle "Error getting queue." $ getQueue α

handle ∷ ToJSON a ⇒ Text → ExceptT Status IO a → IO Response
handle α ω = respond <$> (runExceptT ω)
  where respond = flip textResponse α ||| jsonResponse status200

jsResponse ∷ Status → Text → Response
jsResponse α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, "text/javascript")]
        convert = fromChunks . pure . encodeUtf8

htmlResponse ∷ Status → Text → Response
htmlResponse α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, "text/html")]
        convert = fromChunks . pure . encodeUtf8

textResponse ∷ Status → Text → Response
textResponse α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, "text/plain")]
        convert = fromChunks . pure . encodeUtf8

jsonResponse ∷ ToJSON a ⇒ Status → a → Response
jsonResponse α ω = responseLBS α headers (encode ω)
  where headers = [(hContentType, "application/json")]
