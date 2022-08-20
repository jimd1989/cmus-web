module Main (main) where

import Prelude ((.), ($), pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)

main ∷ MonadIO m ⇒ m ()
main = liftIO $ run 1917 $ \req send → send $ route req 

route ∷ Request → Response
route α = case (pathInfo α, requestMethod α) of
  (_, "POST") → textResponse status200 "hello POST"
  (_, _     ) → textResponse status200 "hello GET"

textResponse ∷ Status → Text → Response
textResponse α ω = responseLBS α [(hContentType, "text/plain")] (convert ω)
  where convert = fromChunks . pure . encodeUtf8
