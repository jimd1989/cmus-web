module Models.LibView (
  LibHead(..), LibLeaf(..), LibNode(..), LibView(..), libView
) where

import Prelude (($))
import Data.Array (groupBy, head)
import Data.Array.NonEmpty (toArray)
import Data.Eq (eq)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Helpers ((∘), (◀), (◁), (●))
import Models.Track (Track)

newtype LibHead = LibHead { children ∷ Array LibView }
newtype LibLeaf = LibLeaf { contents ∷ Array Track }
newtype LibNode = LibNode { children ∷ Array LibView, header ∷ Maybe String }

data LibView = Head LibHead | Leaf LibLeaf | Node LibNode

type Tag = Track → Maybe String

libView ∷ NonEmpty List Tag → Array Track → LibView
libView α ω = Head $ LibHead $ { children: libView' α ω }

libView' ∷ NonEmpty List Tag → Array Track → Array LibView
libView' (t :| α) = (children ∘ toArray) ◁ group
  where group    = groupBy (on eq t)
        children = node α ● (t ◀ head)

node ∷ List Tag → Array Track → Maybe String → LibView
node     Nil ω t = Node $ LibNode { header: t, children: [leaf ω] }
node (α : β) ω t = Node $ LibNode { header: t, children: libView' (α :| β) ω }

leaf ∷ Array Track → LibView
leaf α = Leaf $ LibLeaf { contents: α }
