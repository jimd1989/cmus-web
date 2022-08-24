module Parse where

import Prelude (String, (.), ($), (<>), (<$>), (*>), (<*), id, pure)
import Control.Applicative ((<|>), liftA2)
import Control.Monad ((>=>), (>>=))
import Data.Functor (($>))
import Data.List (drop, foldr)
import Data.Maybe (Maybe(..))
import Data.Text (Text, pack)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text (Parser, anyChar, endOfInput, endOfLine, isEndOfLine, many1, manyTill, manyTill', sepBy, skip, space, string, takeTill)
import Models

tag ∷ Parser ()
tag = string "tag" *> space $> ()

id3 ∷ (Text → InputTrack → InputTrack) → Text → Parser (InputTrack → InputTrack)
id3 f α = f <$> (string α *> space *> takeTill isEndOfLine)

skipField ∷ Parser (InputTrack → InputTrack)
skipField = manyTill anyChar endOfLine $> id

field ∷ Parser (InputTrack → InputTrack)
field =
  (tag *> id3 setInputArtist      "artist")      <|>
  (tag *> id3 setInputTitle       "title")       <|>
  (tag *> id3 setInputAlbum       "album")       <|>
  (tag *> id3 setInputGenre       "genre")       <|>
  (tag *> id3 setInputYear        "year")        <|>
  (tag *> id3 setInputAlbumArtist "albumartist") <|> 
          id3 setInputFilename    "file"         <|>
          id3 setInputDuration    "duration"     <|> skipField

delimiter ∷ Parser ()
delimiter = ((lookAhead $ string "file ") $> ()) <|> endOfInput

-- "file" is always first field. Perform unconditional read for it first,
-- then continue reading every line until the next "file" row is found.
fields ∷ Parser InputTrack
fields = foldr ($) blankTrack <$> liftA2 (:) field (manyTill field delimiter)

tracks ∷ Parser [InputTrack]
tracks = many1 fields <* endOfInput

input ∷ Text
input = "file abc\ntag artist abc\ntag genre rock\nfile xyz\ntag artist xyz\nfile 123\ntag artist 123\n"
