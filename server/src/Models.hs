module Models where

import Prelude (Show)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.HashMap.Strict (HashMap, empty)
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- Global state of cmus library/queue
data Cmus = Cmus {
  files ∷ [Text],
  library ∷ [Track],
  fileNums ∷ HashMap Text Int,
  queue ∷ [Int]
}

cmus ∷ Cmus
cmus = Cmus [] [] empty []

instance ToJSON Cmus where
  toJSON (Cmus _ α _ ω) = object [ "library" .= α, "queue" .= ω]

-- A track in the library; return as compressed tree eventually?
data Track = Track {
  artist ∷ Maybe Text,
  duration ∷ Maybe Text,
  title ∷ Maybe Text,
  num ∷ Int,
  album ∷ Maybe Text,
  genre ∷ Maybe Text,
  year ∷ Maybe Text,
  albumArtist ∷ Maybe Text 
} deriving (Generic, ToJSON, Show)

-- Parsing target for raw cmus data dump
data InputTrack = InputTrack {
  inputArtist ∷ Maybe Text,
  inputDuration ∷ Maybe Text,
  inputTitle ∷ Maybe Text,
  inputFilename ∷ Text,
  inputAlbum ∷ Maybe Text,
  inputGenre ∷ Maybe Text,
  inputYear ∷ Maybe Text,
  inputAlbumArtist ∷ Maybe Text 
}

-- Didn't feel like importing lenses just for this
setInputArtist ∷ Text → InputTrack → InputTrack
setInputArtist α ω = ω { inputArtist = Just α }

setInputDuration ∷ Text → InputTrack → InputTrack
setInputDuration α ω = ω { inputDuration = Just α }

setInputTitle ∷ Text → InputTrack → InputTrack
setInputTitle α ω = ω { inputTitle = Just α }

setInputFilename ∷ Text → InputTrack → InputTrack
setInputFilename α ω = ω { inputFilename = α }

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
  inputFilename = "",
  inputAlbum = Nothing,
  inputGenre = Nothing,
  inputYear = Nothing,
  inputAlbumArtist = Nothing
}
