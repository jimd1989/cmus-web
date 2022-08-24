module Models where

import Prelude (Show)
import Data.Aeson (ToJSON)
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Track = Track {
  artist ∷ Text,
  duration ∷ Int,
  filenum ∷ Int,
  title ∷ Text
} deriving (Generic, ToJSON)

data Album = Album {
  album ∷ Text,
  genre ∷ Text,
  year ∷ Int,
  tracks ∷ [Track]
} deriving (Generic, ToJSON)

data Artist = Artist {
  albumArtist ∷ Text,
  albums ∷ [Album]
} deriving (Generic, ToJSON)

data Cmus = Cmus {
  library ∷ [Artist],
  files ∷ [Text],
  queue ∷ [Track]
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
} deriving Show

setInputArtist α ω = ω { inputArtist = Just α }
setInputDuration α ω = ω { inputDuration = Just α }
setInputTitle α ω = ω { inputTitle = Just α }
setInputFilename α ω = ω { inputFilename = Just α }
setInputAlbum α ω = ω { inputAlbum = Just α }
setInputGenre α ω = ω { inputGenre = Just α }
setInputYear α ω = ω { inputYear = Just α }
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

-- readProcess "cmus-remote" ["-C", "save -q -e -" ] ""

testQueue ∷ [Track]
testQueue = [Track "Throwing Muses" 180 7 "Honeychain"]
