module Models (Album(..), Artist(..), Cmus(..), Track(..), testQueue) where

import Data.Aeson (ToJSON)
import Data.Int (Int)
import Data.Text (Text)
import GHC.Generics (Generic)

data Track = Track {
  artist ∷ Text,
  duration ∷ Int,
  filename ∷ Text,
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
  queue ∷ [Track]
} deriving (Generic, ToJSON)

testQueue ∷ [Track]
testQueue = [Track "Throwing Muses" 180 "honeychain.mp3" "Honeychain"]
