module Parse (parseTracks) where

-- Concerned with parsing newline-delimited `cmus-remote` track info dumps into
-- a list of Track records.

import Prelude (($), (<$>), (*>), (<*), const, id, pure)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither)
import Data.Functor (($>))
import Data.List (foldr)
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text (Parser, endOfInput,
  isEndOfLine, many1, manyTill, parseOnly, space, string, take, takeTill)
import Network.HTTP.Types.Status (Status, status500)
import Models.Track (Track(..), track')

tag ∷ Parser ()
tag = string "tag" *> space $> ()

id3 ∷ (Text → Track → Track) → Text → Parser (Track → Track)
id3 f α = f <$> (string α *> space *> takeTill isEndOfLine)

skipField ∷ Parser (Track → Track)
skipField = takeTill isEndOfLine *> take 1 $> id

file ∷ Parser (Track → Track)
file = id3 (\α ω → ω { filename = α }) "file"

-- Parse order-independent field-modifying lenses, to be selectively applied
-- to default `track'` record. If a field doesn't exist on a track, a 
-- `skipField` lens is generated instead.
field ∷ Parser (Track → Track)
field =
          id3 (\α ω → ω { duration = Just α })    "duration"     <|>
  (tag *> id3 (\α ω → ω { artist = Just α })      "artist"     ) <|>
  (tag *> id3 (\α ω → ω { title = Just α })       "title"      ) <|>
  (tag *> id3 (\α ω → ω { genre = Just α })       "genre"      ) <|>
  (tag *> id3 (\α ω → ω { year = Just α })        "date"       ) <|>
  (tag *> id3 (\α ω → ω { discNumber = Just α })  "discnumber" ) <|>
  (tag *> id3 (\α ω → ω { trackNumber = Just α }) "tracknumber") <|>
  (tag *> id3 (\α ω → ω { albumArtist = Just α }) "albumartist") <|>
  (tag *> id3 (\α ω → ω { album = Just α })       "album"      ) <|> skipField

delimiter ∷ Parser ()
delimiter = ((lookAhead $ string "file ") $> ()) <|> endOfInput

-- "file" is always first field. Perform unconditional read for it first,
-- then continue reading every line until the next "file" row is found. Fold
-- a `track'` record across all the individual field lenses generated to
-- populate the record with all available track info.
fields ∷ Parser Track
fields = foldr ($) track' <$> liftA2 (:) file (manyTill field delimiter)

tracks ∷ Parser [Track]
tracks = many1 fields <* endOfInput

parseTracks ∷ MonadError Status m ⇒ Text → m [Track]
parseTracks "" = pure []
parseTracks α  = liftEither $ left (const status500) $ parseOnly tracks α
