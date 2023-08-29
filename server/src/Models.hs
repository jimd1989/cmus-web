module Models(Cmus(..), Track(..), blankTrack, cmus) where

-- Models cmus state in server memory. Library is represented as a flat list
-- of Track records, keyed directly by array index.

import Prelude (Int, Maybe(..))
import Data.Aeson (ToJSON(..), (.=), object)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Vector as V

data Cmus = Cmus {
  queue ∷ V.Vector Track,     -- Tracks currently queued for listening
  files ∷ H.HashMap Text Int, -- Filename → Track key mapping; for queue reading
  library ∷ V.Vector Track    -- All Tracks available for listening
}

cmus ∷ Cmus
cmus = Cmus V.empty H.empty V.empty

data Track = Track {
  key ∷ Int,
  artist ∷ Maybe Text,
  duration ∷ Maybe Text,
  title ∷ Maybe Text,
  filename ∷ Text,
  album ∷ Maybe Text,
  genre ∷ Maybe Text,
  year ∷ Maybe Text,
  discNumber ∷ Maybe Text,
  trackNumber ∷ Maybe Text,
  albumArtist ∷ Maybe Text
}

blankTrack ∷ Track 
blankTrack = Track {
  key = 0,
  artist = Nothing,
  duration = Nothing,
  title = Nothing,
  filename = "",
  album = Nothing,
  genre = Nothing,
  year = Nothing,
  discNumber = Nothing,
  trackNumber = Nothing,
  albumArtist = Nothing
}

instance ToJSON Cmus where
  toJSON α = object [ "library" .= library α, "queue" .= queue α ]

instance ToJSON Track where
  toJSON α = object [ "key"         .= key α,
                      "artist"      .= artist α, 
                      "len"         .= duration α, 
                      "title"       .= title α, 
                      "album"       .= album α, 
                      "genre"       .= genre α, 
                      "year"        .= year α, 
                      "discN"       .= discNumber α, 
                      "trackN"      .= trackNumber α, 
                      "albumArtist" .= albumArtist α ]

