module Transformers where

import Prelude (Eq, Monoid, (.), ($), (<$>), (==), id, pure)
import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError, throwError)
import Data.Function (on)
import Data.HashMap.Strict (insert, lookup)
import Data.Int (Int)
import Data.List (foldr, groupBy, map, sort, zip)
import Data.Maybe (Maybe, catMaybes, maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (swap, uncurry)
import Data.Vector (empty, fromList)
import Network.HTTP.Types.Status (Status, status500)
import Models

transformLibraryEntry ∷ Int → InputTrack → Cmus → Cmus
transformLibraryEntry n α (Cmus _ _ dict _ tmpFiles tmpTracks) = Cmus {
  files = empty,
  tree = [],
  dict = insert (inputFilename α) (fromInputTrack numberedTrack) dict,
  queue = [],
  tmpTracks = numberedTrack : tmpTracks,
  tmpFiles = (inputFilename α) : tmpFiles
}
  where numberedTrack = setInputFilename "" $ setInputNum n α

transformLibrary ∷ [InputTrack] → Cmus
transformLibrary α = transformFiles $ transformTree flatLibrary
  where flatLibrary = foldr (uncurry transformLibraryEntry) cmus (zip [0 .. ] α)

groupByField ∷ (Eq a, Monoid a) ⇒ (b → Maybe a) → [b] → [(a, [b])]
groupByField field = map format . group
  where group  = groupBy (on (==) field)
        format = traverse swap . catMaybes . map (sequence . (id &&& field))

groupByArtist ∷ [InputTrack] → [(Heading, [InputTrack])]
groupByArtist α = groupByField anyArtist α

transformArtist ∷ (Heading, [InputTrack]) → Artist
transformArtist (α, ω) =
  artist (α, map album $ groupByField inputAlbum ω)

transformTree ∷ Cmus → Cmus
transformTree α = α { 
  tree = map transformArtist $ groupByArtist (sort $ tmpTracks α),
  tmpTracks = []
}

transformFiles ∷ Cmus → Cmus
transformFiles α = α {
  files = fromList $ tmpFiles α,
  tmpFiles = []
}
                    
note ∷ (MonadError Status m) ⇒ Maybe a → m a
note = maybe (throwError status500) pure

find ∷ (MonadError Status m) ⇒ Cmus → InputTrack → m QueuedTrack
find α ω = note $ lookup (inputFilename ω) (dict α)

transformQueue ∷ (MonadError Status m) ⇒ Cmus → [InputTrack] → m Cmus
transformQueue α ω = replaceQueue α <$> newQueue
  where newQueue = traverse (find α) ω
        replaceQueue β γ = β { queue = γ }
