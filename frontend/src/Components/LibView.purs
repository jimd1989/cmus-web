module Components.LibView (renderLibView) where

import Prelude (($), map)
import Data.Array (length)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Helpers ((◇))
import Models.LibView (LibHead(..), LibLeaf(..), LibNode(..), LibView(..))
import Models.Track (Track)

type LibSlot = ∀ q. H.Slot q Void Unit
type Slots = (libHead ∷ LibSlot, libLeaf ∷ LibSlot, libNode ∷ LibSlot)

_libHead = Proxy ∷ Proxy "libHead"
_libLeaf = Proxy ∷ Proxy "libLeaf"
_libNode = Proxy ∷ Proxy "libNode"

-- Fix rendering
libHead ∷ ∀ q o m. H.Component q (LibHead) o m
libHead = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState (LibHead { children }) = { children }
    render { children } = HH.ul_ (map renderLibView children)

libLeaf ∷ ∀ q o m. H.Component q (LibLeaf) o m
libLeaf = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState (LibLeaf { contents }) = { contents }
    render { contents } = HH.ol_ (map (\α → HH.li_ [HH.text $ fromMaybe "" α.title]) contents)

libNode ∷ ∀ q o m. H.Component q (LibNode) o m
libNode = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState (LibNode { children, header }) = { children, header }
    render { children, header } = HH.li_ [HH.ul_ $ [HH.li_ [HH.text $ fromMaybe "" header]] ◇ (map renderLibView children)]
    
renderLibView ∷ ∀ a m. LibView → H.ComponentHTML a Slots m
renderLibView (Head α) = HH.slot_ _libHead unit libHead α
renderLibView (Leaf α) = HH.slot_ _libLeaf unit libLeaf α
renderLibView (Node α) = HH.slot_ _libNode unit libNode α
