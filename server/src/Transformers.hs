module Transformers where

import Data.Int (Int)
import Data.List (foldr, zip)
import Data.HashMap.Strict (insert)
import Data.Tuple (uncurry)
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
