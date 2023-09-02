module Models.Config (Config(..), config') where

import Prelude (($))
import Control.Concurrent.STM.TVar (TVar)
import Control.Applicative (liftA2)
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Models.Cmus (Cmus, cmus')
import Models.Resources (Resources, resources')

data Config = Config {
  cmus ∷ TVar Cmus,
  resources ∷ Resources
}

config' ∷ MonadIO m ⇒ m Config
config' = liftA2 Config (liftIO $ newTVarIO cmus') resources'
