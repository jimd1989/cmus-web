module Components.Menu where

import Prelude (($), map, not)
import Data.Function (const)
import Data.Maybe (fromMaybe)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Helpers ((◇))
import Models.Menu (Header(..), Menu(..), Tracks(..))
import Models.LibView (LibHead(..), LibLeaf(..), LibNode(..), LibView(..))

--tracks ∷ ∀ q i o m. H.Component q i o m


--type LibSlot = ∀ q. H.Slot q Void Unit
--type Slots = (libHead ∷ LibSlot, libLeaf ∷ LibSlot, libNode ∷ LibSlot)
--
--_libHead = Proxy ∷ Proxy "libHead"
--_libLeaf = Proxy ∷ Proxy "libLeaf"
--_libNode = Proxy ∷ Proxy "libNode"
--
---- Fix rendering
--libHead ∷ ∀ q o m. H.Component q (LibHead) o m
--libHead = H.mkComponent { initialState, render, eval }
--  where
--    eval = H.mkEval H.defaultEval
--    initialState (LibHead { children }) = { children }
--    render { children } = HH.ul_ (map renderLibView children)
--
--libLeaf ∷ ∀ q o m. H.Component q (LibLeaf) o m
--libLeaf = H.mkComponent { initialState, render, eval }
--  where
--    eval = H.mkEval H.defaultEval
--    initialState (LibLeaf { contents }) = { contents }
--    render { contents } = HH.ol_ (map (\α → HH.li_ [HH.text $ fromMaybe "" α.title]) contents)
--
--data Action = Hide
--
--libNode ∷ ∀ q o m. H.Component q (LibNode) o m
--libNode = H.mkComponent { initialState, render, eval }
--  where
--    eval = H.mkEval H.defaultEval {
--      handleAction = handleAction
--    }
--    initialState (LibNode { children, header }) = { children, header, hidden: true }
--    render { children, header, hidden } = case hidden of
--      true  → HH.div_   [HH.div [HE.onClick (const Hide)] [HH.text $ fromMaybe "" header]]
--      false → HH.div_ $ [HH.div [HE.onClick (const Hide)] [HH.text $ fromMaybe "" header]] ◇ (map renderLibView children)
--    handleAction Hide = H.modify_ \α → α { hidden = not α.hidden }
--
--renderLibView ∷ ∀ a m. LibView → H.ComponentHTML a Slots m
--renderLibView (Head α) = HH.slot_ _libHead unit libHead α
--renderLibView (Leaf α) = HH.slot_ _libLeaf unit libLeaf α
--renderLibView (Node α) = HH.slot_ _libNode unit libNode α


