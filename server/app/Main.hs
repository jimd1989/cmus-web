module Main (main) where

import Prelude (IO, (.), ($), (>>=), (<$>), flip, pure)
import Control.Arrow ((|||))
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipSettings(..), GzipFiles(..), def, gzip)
import CmusRepository (add, getQueue, play, remove, sync)
import Arguments (Arguments(..), arguments)
import LandingPage (landingPage)
import Models (Cmus, cmus)

main ∷ IO ()
main = do
  args ← arguments
  env  ← newTVarIO $ cmus
  run (port args) $ gzip gzipSettings $ \req send → route req env args >>= send

gzipSettings ∷ GzipSettings
gzipSettings = def { gzipFiles = GzipCompress }

route ∷ Request → TVar Cmus → Arguments → IO Response
route α ω β = case (pathInfo α, requestMethod α) of
  ("add"    : ns : [], "GET") → addTrack ω ns
  ("remove" : n  : [], "GET") → removeTrack ω n
  ("sync"   :      [], "GET") → fullSync ω
  ("queue"  :      [], "GET") → queue ω
  ("app.js" :      [], "GET") → pure $ jsResponse status200 $ js β
  ("play"   :      [], "GET") → play $> textResponse status200 "Toggled."
  ([]                , "GET") → pure $ htmlResponse status200 $ landingPage
  (_                 , _    ) → pure $ textResponse status404 "Invalid path."

addTrack ∷ TVar Cmus → Text → IO Response
addTrack α n = handle "Error adding tracks." $ add α n

removeTrack ∷ TVar Cmus → Text → IO Response
removeTrack α n = handle "Error removing tracks." $ remove α n

fullSync ∷ TVar Cmus → IO Response
fullSync α = handle "Error syncing library." $ sync α

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
