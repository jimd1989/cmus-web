module Resources(Resources(..), resources) where

import Prelude (($), pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text.IO (readFile)

data Resources = Resources { css ∷ Text, html ∷ Text, js ∷ Text }

resources ∷ MonadIO m ⇒ m Resources
resources = liftIO $ do
  c ← readFile "/etc/cmus-web/style.css"
  h ← readFile "/etc/cmus-web/index.html"
  j ← readFile "/etc/cmus-web/index.js"
  pure $ Resources c h j
