module Models where

import Prelude (Int, Show, ($), const)
import Control.Applicative ((<|>))
import Control.Arrow ((|||))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Eq (Eq)
import Data.HashMap.Strict (HashMap, empty)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..), comparing)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Tuple (fst)
import GHC.Generics (Generic)

-- Global state of cmus library/queue
-- Not all fields are exposed; some represent temporary parsing stages
data Cmus = Cmus {
  files ∷ [Text],
  library ∷ [InputTrack],
  tree ∷ [Artist],
  fileNums ∷ HashMap Text Int,
  queue ∷ [Int]
} deriving Show

cmus ∷ Cmus
cmus = Cmus [] [] [] empty []

instance ToJSON Cmus where
  toJSON α = object [ "library" .= tree α, "queue" .= queue α ]

-- Efficient tree view of album artists, falling back to normal artist tag
data Artist = Artist { artistName ∷ Heading, albums ∷ [Album] }
  deriving Show

instance ToJSON Artist where
  toJSON α = object [ "name" .= artistName α, "albums" .= albums α ]

artist ∷ (Heading, [Album]) → Artist
artist (α, ω) = Artist α ω

-- Efficient tree view of albums
data Album = Album { 
  albumTitle ∷ Heading, albumTracks ∷ [InputTrack], albumYear ∷ Int
} deriving Show

instance ToJSON Album where
  toJSON (Album α ω β) = object [ "title" .= α, "tracks" .= ω, "year" .= β ]

album ∷ (Heading, [InputTrack]) → Album
album (α, (x : y)) = Album α (x : y) (inputYear x)
album (α, [])     = Album α [] 0

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
setInputYear α ω = ω { inputYear = convert α }
  where convert β = (const 0 ||| fst) $ decimal β

setInputDiscNumber ∷ Text → InputTrack → InputTrack
setInputDiscNumber α ω = ω { inputDiscNumber = convert α }
  where convert β = (const 0 ||| fst) $ decimal β

setInputTrackNumber ∷ Text → InputTrack → InputTrack
setInputTrackNumber α ω = ω { inputTrackNumber = convert α }
  where convert β = (const 0 ||| fst) $ decimal β

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
