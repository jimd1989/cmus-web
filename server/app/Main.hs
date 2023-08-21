module Main (main) where

import Prelude (Int, IO, (.), ($), (>>=), (<$>), const, flip, id, pure)
import Control.Arrow ((|||))
import Control.Concurrent.STM (newTVarIO, readTVarIO)
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
import CmusRepository (add, getQueue, play, remove, sync)
import Helpers ((◀), (◁), head', readInt)
import Models (Cmus, cmus)

main ∷ IO ()
main = do
  env     ← newTVarIO $ cmus
  portNum ← port
  run portNum $ gzip gzipSettings $ \req send → route req env >>= send

port ∷ IO Int
port = (extract . parseHead) <$> getArgs
  where parseHead = readInt ◀ pack ◁ head'
        extract   = const 1917 ||| id

gzipSettings ∷ GzipSettings
gzipSettings = def { gzipFiles = GzipCompress }

route ∷ Request → TVar Cmus → IO Response
route α ω = case (pathInfo α, requestMethod α) of
  ("add"    : ns : [], "GET"   ) → addTrack ω ns
  ("remove" : n  : [], "GET"   ) → removeTrack ω n
  ("sync"   :      [], "GET"   ) → fullSync ω
  ("queue"  :      [], "GET"   ) → queue ω
  ("play"   :      [], "GET"   ) → play $> textResponse status200 "Toggled."
  ([]                , "GET"   ) → jsonResponse status200 <$> readTVarIO ω
  (_                 , _       ) → pure $ textResponse status404 "Invalid path."


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

textResponse ∷ Status → Text → Response
textResponse α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, "text/plain")]
        convert = fromChunks . pure . encodeUtf8

jsonResponse ∷ ToJSON a ⇒ Status → a → Response
jsonResponse α ω = responseLBS α headers (encode ω)
  where headers = [(hContentType, "application/json")]
