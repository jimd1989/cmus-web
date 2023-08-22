module Arguments (Arguments(..), arguments) where

import Prelude (Int, Show, ($), (<$>), (=<<), error)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import System.Environment (getArgs)
import Helpers (readInt')

data Arguments = Arguments { port ∷ Int, js ∷ Text } deriving Show

arguments ∷ MonadIO m ⇒ m Arguments
arguments = liftIO $ check =<< getArgs
  where check (α : ω : []) = Arguments (readInt' 1917 $ pack α) <$> readFile ω
        check  _           = error "usage: port-num frontend-path"
