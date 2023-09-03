module Models.Cmus (Cmus(..), parseCmus) where

import Prelude (($))
import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Either (Either(..))
import Simple.JSON (readJSON)
import Helpers ((◇))
import Models.Track (Track)

type Cmus = {
 library ∷ Array Track
}

parseCmus ∷ ∀ m. MonadError String m ⇒ String → m Cmus
parseCmus α = liftEither $ case readJSON α of
  Right (r ∷ Cmus) → Right r
  Left _           → Left $ "Error parsing " ◇ α


