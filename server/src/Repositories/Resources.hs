module Repositories.Resources (staticCss, staticHtml, staticJs) where

import Prelude ((.), (<$>))
import Control.Monad.Reader (MonadReader, ask)
import Data.Text (Text)
import Models.Config (Config(..))
import Models.Resources (Resources(..))

staticCss ∷ MonadReader Config m ⇒ m Text
staticCss = (css . resources) <$> ask 

staticHtml ∷ MonadReader Config m ⇒ m Text
staticHtml = (html . resources) <$> ask 

staticJs ∷ MonadReader Config m ⇒ m Text
staticJs = (js . resources) <$> ask 
