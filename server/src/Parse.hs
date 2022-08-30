module Parse (parseCmus) where

-- Reads newline-delimited cmus output into InputTrack records: a primitive
-- representation of the cmus library that is refined further by transformers.

import Prelude (($), (<$>), (*>), (<*), const, id, pure)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither)
import Data.Functor (($>))
import Data.List (foldr)
import Data.Text (Text)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text (Parser, endOfInput,
  isEndOfLine, many1, manyTill, parseOnly, space, string, take, takeTill)
import Network.HTTP.Types.Status (Status, status500)
import Models (InputTrack, blankTrack, setInputAlbum, setInputAlbumArtist,
  setInputArtist, setInputDiscNumber, setInputDuration, setInputFilename, 
  setInputGenre, setInputTitle, setInputTrackNumber, setInputYear)

tag ∷ Parser ()
tag = string "tag" *> space $> ()

id3 ∷ (Text → InputTrack → InputTrack) → Text → Parser (InputTrack → InputTrack)
id3 f α = f <$> (string α *> space *> takeTill isEndOfLine)

skipField ∷ Parser (InputTrack → InputTrack)
skipField = takeTill isEndOfLine *> take 1 $> id

file ∷ Parser (InputTrack → InputTrack)
file = id3 setInputFilename "file"

field ∷ Parser (InputTrack → InputTrack)
field =
  (tag *> id3 setInputArtist      "artist")      <|>
  (tag *> id3 setInputTitle       "title")       <|>
  (tag *> id3 setInputAlbum       "album")       <|>
  (tag *> id3 setInputGenre       "genre")       <|>
  (tag *> id3 setInputYear        "date")        <|>
  (tag *> id3 setInputDiscNumber  "discnumber")  <|>
  (tag *> id3 setInputTrackNumber "tracknumber") <|>
  (tag *> id3 setInputAlbumArtist "albumartist") <|> 
          id3 setInputDuration    "duration"     <|> skipField

delimiter ∷ Parser ()
delimiter = ((lookAhead $ string "file ") $> ()) <|> endOfInput

-- "file" is always first field. Perform unconditional read for it first,
-- then continue reading every line until the next "file" row is found.
fields ∷ Parser InputTrack
fields = foldr ($) blankTrack <$> liftA2 (:) file (manyTill field delimiter)

tracks ∷ Parser [InputTrack]
tracks = many1 fields <* endOfInput

parseCmus ∷ MonadError Status m ⇒ Text → m [InputTrack]
parseCmus "" = pure []
parseCmus α  = liftEither $ left (const status500) $ parseOnly tracks α
