module Models where

import Prelude (Show)
import Data.Aeson (ToJSON)
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Cmus = Cmus {
  queue ∷ [InputTrack]
} deriving (Generic, ToJSON)

data InputTrack = InputTrack {
  inputArtist ∷ Maybe Text,
  inputDuration ∷ Maybe Text,
  inputTitle ∷ Maybe Text,
  inputFilename ∷ Maybe Text,
  inputAlbum ∷ Maybe Text,
  inputGenre ∷ Maybe Text,
  inputYear ∷ Maybe Text,
  inputAlbumArtist ∷ Maybe Text 
} deriving (Generic, ToJSON, Show)

-- Didn't feel like importing lenses just for this

setInputArtist ∷ Text → InputTrack → InputTrack
setInputArtist α ω = ω { inputArtist = Just α }

setInputDuration ∷ Text → InputTrack → InputTrack
setInputDuration α ω = ω { inputDuration = Just α }

setInputTitle ∷ Text → InputTrack → InputTrack
setInputTitle α ω = ω { inputTitle = Just α }

setInputFilename ∷ Text → InputTrack → InputTrack
setInputFilename α ω = ω { inputFilename = Just α }

setInputAlbum ∷ Text → InputTrack → InputTrack
setInputAlbum α ω = ω { inputAlbum = Just α }

setInputGenre ∷ Text → InputTrack → InputTrack
setInputGenre α ω = ω { inputGenre = Just α }

setInputYear ∷ Text → InputTrack → InputTrack
setInputYear α ω = ω { inputYear = Just α }

setInputAlbumArtist ∷ Text → InputTrack → InputTrack
setInputAlbumArtist α ω = ω { inputAlbumArtist = Just α }

blankTrack ∷ InputTrack 
blankTrack = InputTrack {
  inputArtist = Nothing,
  inputDuration = Nothing,
  inputTitle = Nothing,
  inputFilename = Nothing,
  inputAlbum = Nothing,
  inputGenre = Nothing,
  inputYear = Nothing,
  inputAlbumArtist = Nothing
}
