module Models.LibView (LibHead(..), LibLeaf(..), LibNode(..), LibView(..), libView) where

import Prelude (($), (=<<), map)
import Data.Array (groupBy, head)
import Data.Array.NonEmpty (toArray)
import Data.Eq (eq)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Helpers ((∘))
import Models.Track (Track)

newtype LibHead = LibHead { children ∷ Array LibView }
newtype LibLeaf = LibLeaf { contents ∷ Array Track }
newtype LibNode = LibNode { children ∷ Array LibView, header ∷ Maybe String }

data LibView = Head LibHead | Leaf LibLeaf | Node LibNode

type Tag = Track → Maybe String

libView ∷ NonEmpty List Tag → Array Track → LibView
libView α ω = Head $ LibHead $ { children: libChildren α ω }

libChildren ∷ NonEmpty List Tag → Array Track → Array LibView
libChildren (t :| α) ω = map (libView' (t :| α) ∘ toArray) $ groupBy (on eq t) ω

libView' ∷ NonEmpty List Tag → Array Track → LibView
libView' (_ :|   Nil) ω = Leaf $ LibLeaf { contents: ω }
libView' (t :| α : β) ω = Node $ LibNode {
  header: t =<< head ω,
  children: libChildren (α :| β) ω
}
