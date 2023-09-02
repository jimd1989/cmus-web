module Models.Track (Track(..), track') where

import Prelude (Int, Maybe(..))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)

data Track = Track {
  key ∷ Int,
  artist ∷ Maybe Text,
  duration ∷ Maybe Text,  -- Raw parsed duration, in seconds
  duration' ∷ Maybe Text, -- Refined duration, in mm:ss
  title ∷ Maybe Text,
  filename ∷ Text,
  album ∷ Maybe Text,
  genre ∷ Maybe Text,
  year ∷ Maybe Text,
  discNumber ∷ Maybe Text,
  trackNumber ∷ Maybe Text,
  albumArtist ∷ Maybe Text
}

track' ∷ Track 
track' = Track {
  key = 0,
  artist = Nothing,
  duration = Nothing,
  duration' = Nothing,
  title = Nothing,
  filename = "",
  album = Nothing,
  genre = Nothing,
  year = Nothing,
  discNumber = Nothing,
  trackNumber = Nothing,
  albumArtist = Nothing
}

instance ToJSON Track where
  toJSON α = object [ "key"         .= key α,
                      "artist"      .= artist α, 
                      "len"         .= duration' α, 
                      "title"       .= title α, 
                      "album"       .= album α, 
                      "genre"       .= genre α, 
                      "year"        .= year α, 
                      "discN"       .= discNumber α, 
                      "trackN"      .= trackNumber α, 
                      "albumArtist" .= albumArtist α ]

