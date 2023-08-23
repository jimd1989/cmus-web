module Resources(Resources(..), resources) where

import Prelude (($), pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text.IO (readFile)

data Resources = Resources { html ∷ Text, js ∷ Text }

resources ∷ MonadIO m ⇒ m Resources
resources = liftIO $ do
  h ← readFile "/etc/cmus-web/index.html"
  j ← readFile "/etc/cmus-web/index.js"
  pure $ Resources h j
