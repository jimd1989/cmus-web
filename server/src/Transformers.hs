module Transformers where

import Prelude (Eq, (.), ($), (<$>), (==), (<>), id, otherwise, pure)
import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError, throwError)
import Data.HashMap.Strict (insert, lookup)
import Data.Int (Int)
import Data.List (foldr, map, sort, zip)
import Data.Maybe (Maybe, catMaybes, maybe)
import Data.Text (Text)
import Data.Traversable (sequence, traverse)
import Data.Tuple (swap, uncurry)
import Network.HTTP.Types.Status (Status, status500)
import Models

transformLibraryEntry ∷ Int → InputTrack → Cmus → Cmus
transformLibraryEntry n α (Cmus files library _ fileNums _ ) = Cmus {
  files = (inputFilename α) : files,
  library = (setInputNum n α) : library,
  tree = [],
  fileNums = insert (inputFilename α) n fileNums,
  queue = []
}

transformLibrary ∷ [InputTrack] → Cmus
transformLibrary α = transformTree flatLibrary
  where flatLibrary = foldr (uncurry transformLibraryEntry) cmus (zip [0 .. ] α)

-- alphabetizing error (?) Manually fixed with `sort` for now
add' ∷ Eq a ⇒ [(a, [b])] → a → b → [(a, [b])] → [(a, [b])]
add' α β γ [] = (β, [γ]) : α
add' α β γ ((x, y) : ω)
    | β == x = ((x, γ : y) : α) <> ω
    | otherwise = add' ((x, y) : α) β γ ω

add ∷ Eq a ⇒ a → b → [(a, [b])] → [(a, [b])]
add = add' []

groupByField ∷ Eq a ⇒ (b → Maybe a) → [b] → [(a, [b])]
groupByField field α = foldr (uncurry add) [] ω
  where ω = map swap $ catMaybes $ map (sequence . (id &&& field)) α

groupByArtist ∷ [InputTrack] → [(Text, [InputTrack])]
groupByArtist α = groupByField anyArtist α

transformArtist ∷ (Text, [InputTrack]) → Artist
transformArtist (α, ω) =
  artist (α, map transformAlbum $ groupByField inputAlbum ω)

transformAlbum ∷ (Text, [InputTrack]) → Album
transformAlbum (α, ω) = album (α, sort ω)

transformTree ∷ Cmus → Cmus
transformTree α = α { 
  tree = sort $ map transformArtist $ groupByArtist (library α),
  library = []
}
                    
note ∷ (MonadError Status m) ⇒ Maybe a → m a
note = maybe (throwError status500) pure

find ∷ (MonadError Status m) ⇒ Cmus → InputTrack → m Int
find α ω = note $ lookup (inputFilename ω) (fileNums α)

transformQueue ∷ (MonadError Status m) ⇒ Cmus → [InputTrack] → m Cmus
transformQueue α ω = replaceQueue α <$> newQueue
  where newQueue = traverse (find α) ω
        replaceQueue β γ = β { queue = γ }
