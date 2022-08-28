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
import Data.Vector (fromList)
import Network.HTTP.Types.Status (Status, status500)
import Models

transformLibraryEntry ∷ Int → InputTrack → Cmus → Cmus
transformLibraryEntry n α (Cmus files _ library _ fileNums _ ) = Cmus {
  filesList = (inputFilename α) : files,
  library = (setInputNum n α) : library,
  tree = [],
  fileNums = insert (inputFilename α) n fileNums,
  queue = []
}

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
  tree = map transformArtist $ groupByArtist (sort $ library α),
  library = []
}

transformFiles ∷ Cmus → Cmus
transformFiles α = α {
  files = fromList $ filesList α,
  filesList = []
}
                    
note ∷ (MonadError Status m) ⇒ Maybe a → m a
note = maybe (throwError status500) pure

find ∷ (MonadError Status m) ⇒ Cmus → InputTrack → m Int
find α ω = note $ lookup (inputFilename ω) (fileNums α)

transformQueue ∷ (MonadError Status m) ⇒ Cmus → [InputTrack] → m Cmus
transformQueue α ω = replaceQueue α <$> newQueue
  where newQueue = traverse (find α) ω
        replaceQueue β γ = β { queue = γ }
