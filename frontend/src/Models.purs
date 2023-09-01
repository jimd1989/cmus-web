module Models (Cmus(..), Track(..), parseCmus, parseTracks) where

import Prelude (($))
import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Simple.JSON (readJSON)
import Helpers ((◇))

type Cmus = {
 library ∷ Array Track
}

parseCmus ∷ ∀ m. MonadError String m ⇒ String → m Cmus
parseCmus α = liftEither $ case readJSON α of
  Right (r ∷ Cmus) → Right r
  Left _           → Left $ "Error parsing " ◇ α

type Track = {
  key ∷ Int,
  artist ∷ Maybe String,
  len ∷ Maybe String,
  title ∷ Maybe String,
  album ∷ Maybe String,
  genre ∷ Maybe String,
  year ∷ Maybe String,
  discN ∷ Maybe String,
  trackN ∷ Maybe String,
  albumArtist ∷ Maybe String
}

parseTracks ∷ ∀ m. MonadError String m ⇒ String → m (Array Track)
parseTracks α = liftEither $ case readJSON α of
  Right (r ∷ Array Track) → Right r
  Left _                  → Left $ "Error parsing " ◇ α
