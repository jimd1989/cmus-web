module Models where

import Prelude (Int, ($), (<>), const)
import Control.Applicative ((<|>))
import Control.Arrow ((|||))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Eq (Eq)
import Data.HashMap.Strict (HashMap, empty)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..), comparing)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Tuple (fst)

-- Global state of cmus library/queue
-- Not all fields are exposed; some represent temporary parsing stages
data Cmus = Cmus {
  files ∷ [Text],
  library ∷ [InputTrack],
  tree ∷ [Artist],
  fileNums ∷ HashMap Text Int,
  queue ∷ [Int]
}

cmus ∷ Cmus
cmus = Cmus [] [] [] empty []

instance ToJSON Cmus where
  toJSON α = object [ "library" .= tree α, "queue" .= queue α ]

-- Efficient tree view of album artists, falling back to normal artist tag
data Artist = Artist { artistName ∷ Text, albums ∷ [Album] }
  deriving (Eq)

instance Ord Artist where
  compare = comparing artistName

instance ToJSON Artist where
  toJSON α = object [ "name" .= artistName α, "albums" .= albums α ]

artist ∷ (Text, [Album]) → Artist
artist (α, ω) = Artist α ω

-- Efficient tree view of albums
data Album = Album { 
  albumTitle ∷ Text, albumTracks ∷ [InputTrack], albumYear ∷ Int
} deriving Eq

instance Ord Album where
  compare = comparing albumYear

instance ToJSON Album where
  toJSON (Album α ω β) = object [ "title" .= α, "tracks" .= ω, "year" .= β ]

album ∷ (Text, [InputTrack]) → Album
album (α, (x : y)) = Album α (x : y) (inputYear x)
album (α, [])     = Album α [] 0

-- Parsing target for raw cmus data dump
data InputTrack = InputTrack {
  inputArtist ∷ Maybe Text,
  inputDuration ∷ Maybe Text,
  inputTitle ∷ Maybe Text,
  inputFilename ∷ Text,
  inputNum ∷ Maybe Int,
  inputAlbum ∷ Maybe Text,
  inputGenre ∷ Maybe Text,
  inputYear ∷ Int,
  inputDiscNumber ∷ Int,
  inputTrackNumber ∷ Int,
  inputAlbumArtist ∷ Maybe Text 
} deriving Eq

instance Ord InputTrack where
  compare = comparing inputDiscNumber <> comparing inputTrackNumber

instance ToJSON InputTrack where
  toJSON α = object [
    "performer" .= inputAlbumArtist α,
    "length"    .= inputDuration α,
    "key"       .= inputNum α,
    "title"     .= inputTitle α ]

-- Didn't feel like importing lenses just for this
anyArtist ∷ InputTrack → Maybe Text
anyArtist α = inputAlbumArtist α <|> inputArtist α

setInputArtist ∷ Text → InputTrack → InputTrack
setInputArtist α ω = ω { inputArtist = Just α }

setInputDuration ∷ Text → InputTrack → InputTrack
setInputDuration α ω = ω { inputDuration = Just α }

setInputTitle ∷ Text → InputTrack → InputTrack
setInputTitle α ω = ω { inputTitle = Just α }

setInputFilename ∷ Text → InputTrack → InputTrack
setInputFilename α ω = ω { inputFilename = α }

setInputNum ∷ Int → InputTrack → InputTrack
setInputNum α ω = ω { inputNum = Just α }

setInputAlbum ∷ Text → InputTrack → InputTrack
setInputAlbum α ω = ω { inputAlbum = Just α }

setInputGenre ∷ Text → InputTrack → InputTrack
setInputGenre α ω = ω { inputGenre = Just α }

setInputYear ∷ Text → InputTrack → InputTrack
setInputYear α ω = ω { inputYear = convert α }
  where convert β = (const 0 ||| fst) $ decimal β

setInputDiscNumber ∷ Text → InputTrack → InputTrack
setInputDiscNumber α ω = ω { inputYear = convert α }
  where convert β = (const 0 ||| fst) $ decimal β

setInputTrackNumber ∷ Text → InputTrack → InputTrack
setInputTrackNumber α ω = ω { inputYear = convert α }
  where convert β = (const 0 ||| fst) $ decimal β

setInputAlbumArtist ∷ Text → InputTrack → InputTrack
setInputAlbumArtist α ω = ω { inputAlbumArtist = Just α }

blankTrack ∷ InputTrack 
blankTrack = InputTrack {
  inputArtist = Nothing,
  inputDuration = Nothing,
  inputTitle = Nothing,
  inputFilename = "",
  inputNum = Nothing,
  inputAlbum = Nothing,
  inputGenre = Nothing,
  inputYear = 0,
  inputDiscNumber = 0,
  inputTrackNumber = 0,
  inputAlbumArtist = Nothing
}
