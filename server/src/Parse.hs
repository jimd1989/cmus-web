module Parse (parseCmus) where

import Prelude ((.), ($), (<$>), (*>), (<*), const, id)
import Control.Applicative ((<|>), liftA2)
import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither)
import Data.Functor (($>))
import Data.List (foldr)
import Data.Text (Text)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text (Parser, anyChar, endOfInput, endOfLine, 
  isEndOfLine, many1, manyTill, parseOnly, space, string, takeTill)
import Models (InputTrack, blankTrack, setInputAlbum, 
  setInputAlbumArtist, setInputArtist, setInputDuration, setInputFilename, 
  setInputGenre, setInputTitle, setInputYear)
import Network.HTTP.Types.Status (Status, status500)

tag ∷ Parser ()
tag = string "tag" *> space $> ()

id3 ∷ (Text → InputTrack → InputTrack) → Text → Parser (InputTrack → InputTrack)
id3 f α = f <$> (string α *> space *> takeTill isEndOfLine)

skipField ∷ Parser (InputTrack → InputTrack)
skipField = manyTill anyChar endOfLine $> id

file ∷ Parser (InputTrack → InputTrack)
file = id3 setInputFilename "file"

field ∷ Parser (InputTrack → InputTrack)
field =
  (tag *> id3 setInputArtist      "artist")      <|>
  (tag *> id3 setInputTitle       "title")       <|>
  (tag *> id3 setInputAlbum       "album")       <|>
  (tag *> id3 setInputGenre       "genre")       <|>
  (tag *> id3 setInputYear        "date")        <|>
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
parseCmus = liftEither . left (const status500) . parseOnly tracks
