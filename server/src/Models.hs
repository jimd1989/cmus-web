module Models (
  Artist, Cmus(..), Heading, InputTrack(..), QueuedTrack(..), album, anyArtist, 
  artist, blankTrack, cmus, fromInputTrack, setInputAlbum, setInputAlbumArtist,
  setInputArtist, setInputDiscNumber, setInputDuration, setInputFilename, 
  setInputGenre, setInputNum, setInputTitle, setInputTrackNumber, setInputYear
) where

import Prelude (Int, Show, ($))
import Control.Applicative ((<|>))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Eq (Eq)
import Data.HashMap.Strict (HashMap, empty)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..), comparing)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Tuple (uncurry)
import Data.Vector (Vector, fromList)
import GHC.Generics (Generic)
import Helpers (readInt')

-- Global state of cmus library/queue
-- Not all fields are exposed; some represent temporary parsing stages
data Cmus = Cmus {
  files ∷ Vector Text,             -- Flat vector of all filenames
  tree ∷ [Artist],                 -- Tree view of library
  dict ∷ HashMap Text QueuedTrack, -- Filename → Track dictionary (for queue)
  queue ∷ [QueuedTrack],           -- Currently queued tracks
  tmpFiles ∷ [Text],               -- only used in parsing
  tmpTracks ∷ [InputTrack]         -- only used in parsing
} deriving Show

cmus ∷ Cmus
cmus = Cmus (fromList []) [] empty [] [] []

instance ToJSON Cmus where
  toJSON α = object [ "library" .= tree α, "queue" .= queue α ]

-- Efficient tree view of album artists, falling back to normal artist tag
data Artist = Artist { artistName ∷ Heading, albums ∷ [Album] }
  deriving Show

instance ToJSON Artist where
  toJSON α = object [ "name" .= artistName α, "albums" .= albums α ]

artist ∷ (Heading, [Album]) → Artist
artist = uncurry Artist

-- Efficient tree view of albums
data Album = Album { 
  albumTitle ∷ Heading, albumTracks ∷ [InputTrack], albumYear ∷ Int
} deriving Show

instance ToJSON Album where
  toJSON (Album α β ω) = object [ "title" .= α, "tracks" .= β, "year" .= ω ]

album ∷ (Heading, [InputTrack]) → Album
album (α, (x : y)) = Album α (x : y) (inputYear x)
album (α, [])      = Album α [] 0

-- Efficient view of a queued track
data QueuedTrack = QueuedTrack {
  queuedArtist ∷ Maybe Heading,
  queuedTitle ∷ Maybe Text,
  queuedDuration ∷ Maybe Text
} deriving Show

instance ToJSON QueuedTrack where
  toJSON (QueuedTrack α β ω) = 
    object [ "title" .= α, "performer" .= β, "duration" .= ω ]

fromInputTrack ∷ InputTrack → QueuedTrack
fromInputTrack α = QueuedTrack (inputArtist α) (inputTitle α) (inputDuration α)

-- Heading type: text that can be grouped under
newtype Heading = Heading Text
  deriving (Generic, Eq, Ord, ToJSON, Show)

instance Monoid Heading where
  mempty = (Heading "")

instance Semigroup Heading where
  α <> _ = α

-- Parsing target for raw cmus data dump
data InputTrack = InputTrack {
  inputArtist ∷ Maybe Heading,
  inputDuration ∷ Maybe Text,
  inputTitle ∷ Maybe Text,
  inputFilename ∷ Text,
  inputNum ∷ Maybe Int,
  inputAlbum ∷ Maybe Heading,
  inputGenre ∷ Maybe Text,
  inputYear ∷ Int,
  inputDiscNumber ∷ Int,
  inputTrackNumber ∷ Int,
  inputAlbumArtist ∷ Maybe Heading 
} deriving (Eq, Show)

instance Ord InputTrack where
  compare = comparing anyArtist        <> 
            comparing inputYear        <>
            comparing inputAlbum       <>
            comparing inputDiscNumber  <>
            comparing inputTrackNumber

instance ToJSON InputTrack where
  toJSON α = object [
    "performer" .= inputAlbumArtist α,
    "length"    .= inputDuration α,
    "key"       .= inputNum α,
    "title"     .= inputTitle α ]

-- Didn't feel like importing lenses just for this
anyArtist ∷ InputTrack → Maybe Heading
anyArtist α = inputAlbumArtist α <|> inputArtist α

setInputArtist ∷ Text → InputTrack → InputTrack
setInputArtist α ω = ω { inputArtist = Just $ Heading α }

setInputDuration ∷ Text → InputTrack → InputTrack
setInputDuration α ω = ω { inputDuration = Just α }

setInputTitle ∷ Text → InputTrack → InputTrack
setInputTitle α ω = ω { inputTitle = Just α }

setInputFilename ∷ Text → InputTrack → InputTrack
setInputFilename α ω = ω { inputFilename = α }

setInputNum ∷ Int → InputTrack → InputTrack
setInputNum α ω = ω { inputNum = Just α }

setInputAlbum ∷ Text → InputTrack → InputTrack
setInputAlbum α ω = ω { inputAlbum = Just $ Heading α }

setInputGenre ∷ Text → InputTrack → InputTrack
setInputGenre α ω = ω { inputGenre = Just α }

setInputYear ∷ Text → InputTrack → InputTrack
setInputYear α ω = ω { inputYear = readInt' 0 α }

setInputDiscNumber ∷ Text → InputTrack → InputTrack
setInputDiscNumber α ω = ω { inputDiscNumber = readInt' 0 α }

setInputTrackNumber ∷ Text → InputTrack → InputTrack
setInputTrackNumber α ω = ω { inputTrackNumber = readInt' 0 α }

setInputAlbumArtist ∷ Text → InputTrack → InputTrack
setInputAlbumArtist α ω = ω { inputAlbumArtist = Just $ Heading α }

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
