module Transformers where

import Prelude (($), (<$>), pure, traverse)
import Control.Monad.Except (MonadError, throwError)
import Data.HashMap.Strict (insert, lookup)
import Data.Int (Int)
import Data.List (foldr, zip)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (uncurry)
import Network.HTTP.Types.Status (Status, status500)
import Models

transformTrack ∷ Int → InputTrack → Track
transformTrack n α = Track {
  artist = inputArtist α,
  duration = inputDuration α,
  title = inputTitle α ,
  num = n,
  album = inputAlbum α,
  genre = inputGenre α,
  year = inputYear α,
  albumArtist = inputAlbumArtist α 
}

transformLibraryEntry ∷ Int → InputTrack → Cmus → Cmus
transformLibraryEntry n α (Cmus files library fileNums _ ) = Cmus {
  files = (inputFilename α) : files,
  library = (transformTrack n α) : library,
  fileNums = insert (inputFilename α) n fileNums,
  queue = []
}

transformLibrary ∷ [InputTrack] → Cmus
transformLibrary α = foldr (uncurry transformLibraryEntry) cmus (zip [0 .. ] α)

note ∷ (MonadError Status m) ⇒ Maybe a → m a
note = maybe (throwError status500) pure

find ∷ (MonadError Status m) ⇒ Cmus → InputTrack → m Int
find α ω = note $ lookup (inputFilename ω) (fileNums α)

transformQueue ∷ (MonadError Status m) ⇒ Cmus → [InputTrack] → m Cmus
transformQueue α ω = replaceQueue α <$> newQueue
  where newQueue = traverse (find α) ω
        replaceQueue β γ = β { queue = γ }
