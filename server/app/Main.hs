module Main (main) where

import Prelude (IO, (.), ($), (>>=), (<$>), flip, pure)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Concurrent.STM.TVar (TVar)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipSettings(..), GzipFiles(..), def, gzip)
import Models (Cmus, cmus)

import CmusRepository
import Control.Arrow
import Control.Monad.Except (runExceptT)

main ∷ IO ()
main = do
  env ← newTVarIO $ cmus
  run 1917 $ gzip settings $ \req send → route req env >>= send

settings ∷ GzipSettings
settings = def { gzipFiles = GzipCompress }

route ∷ Request → TVar Cmus → IO Response
route α ω = case (pathInfo α, requestMethod α) of
  ("add"    : ns : [], "POST"  ) → addTrack ω ns
  ("remove" : n  : [], "DELETE") → fullSync ω -- Not implemented
  ("sync"   :      [], "GET"   ) → fullSync ω
  ("queue"  :      [], "GET"   ) → queue ω
  ([]                , "GET"   ) → jsonResponse status200 <$> readTVarIO ω
  (_                 , _       ) → pure $ textResponse status404 "Invalid path."


addTrack ∷ TVar Cmus → Text → IO Response
addTrack α n = handle <$> (runExceptT $ add α n)
  where handle = flip textResponse "" ||| jsonResponse status200

fullSync ∷ TVar Cmus → IO Response
fullSync α = handle <$> (runExceptT $ sync α)
  where handle = flip textResponse "" ||| jsonResponse status200

queue ∷ TVar Cmus → IO Response
queue α = handle <$> (runExceptT $ getQueue α)
  where handle = flip textResponse "" ||| jsonResponse status200

textResponse ∷ Status → Text → Response
textResponse α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, "text/plain")]
        convert = fromChunks . pure . encodeUtf8

jsonResponse ∷ ToJSON a ⇒ Status → a → Response
jsonResponse α ω = responseLBS α headers (encode ω)
  where headers = [(hContentType, "application/json")]
