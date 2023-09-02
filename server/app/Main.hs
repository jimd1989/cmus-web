module Main (main) where

import Prelude (Int, IO, (.), ($), (>>=), (<$>), const, id)
import Control.Arrow ((|||))
import Control.Monad.Reader (runReaderT)
import Data.Text (pack)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipSettings(..), GzipFiles(..), def, gzip)
import System.Environment (getArgs)
import Helpers ((◀), (◁), head', readInt)
import Models.Config (Config, config')
import Routes (routes)

main ∷ IO ()
main = do
  port ← portNumber
  conf ← config'
  run port $ gzip gzipSettings $ app conf

app ∷ Config → Application
app α req send = runReaderT (routes req) α >>= send

gzipSettings ∷ GzipSettings
gzipSettings = def { gzipFiles = GzipCompress }

portNumber ∷ IO Int
portNumber = (const 1917 ||| id) . (readInt ◀ pack ◁ head') <$> getArgs
