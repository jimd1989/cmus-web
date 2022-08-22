module Main (main) where

import Prelude (IO, (.), ($), (>>=), (<$>), pure)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.STM (atomically)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Models (Cmus(..), testQueue)

main ∷ IO ()
main = do
  env ← newTVarIO $ Cmus { library = [], queue = [] } 
  run 1917 $ \req send → route req env >>= send

route ∷ Request → TVar Cmus → IO Response
route α ω = case (pathInfo α, requestMethod α) of
  (_,             "POST") → pure $ textResponse status200 "hello POST"
  ("change" : [], "GET" ) → jsonResponse status200 <$> addToQueue ω
  (_,             _     ) → jsonResponse status200 <$> readTVarIO ω

addToQueue ∷ TVar Cmus → IO Cmus
addToQueue α = atomically $ do
  cmus ← readTVar α
  let newCmus = cmus { queue = testQueue }
  writeTVar α newCmus
  pure newCmus

textResponse ∷ Status → Text → Response
textResponse α ω = responseLBS α headers (convert ω)
  where headers = [(hContentType, "text/plain")]
        convert = fromChunks . pure . encodeUtf8

jsonResponse ∷ ToJSON a ⇒ Status → a → Response
jsonResponse α ω = responseLBS α headers (encode ω)
  where headers = [(hContentType, "application/json")]
