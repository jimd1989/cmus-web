module CmusRepository where

import Prelude (String, ($), (<$>), (>>=))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack)
import Network.HTTP.Types.Status (Status)
import System.Process (readProcess)
import Models (InputTrack)
import Parse (parseCmus)

readCmus ∷ MonadIO m ⇒ [String] → m Text
readCmus α = liftIO $ pack <$> readProcess "cmus-remote" α ""

readQueue ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readQueue = readCmus ["-C", "save -q -e -"] >>= parseCmus


