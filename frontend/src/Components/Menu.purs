module Components.Menu where

import Prelude (($), (>>=), identity, map, not, pure)
import Data.Array (fromFoldable)
import Data.Function (const)
import Data.Map (update, values)
import Data.Maybe (fromMaybe)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Helpers ((◇), (∘))
import Models.Menu (Header(..), Menu(..), Tracks(..))
import Models.Track (Track)
import Models.LibView (LibHead(..), LibLeaf(..), LibNode(..), LibView(..))

tracks ∷ ∀ q o m. H.Component q Tracks o m
tracks = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState = identity
    render (Tracks { contents }) = HH.ul_ $ map renderTrack contents

renderTrack ∷ ∀ a m. Track → H.ComponentHTML a () m
renderTrack α = HH.li_ [HH.text $ fromMaybe "" α.title]

data ParentAction = Toggle | Update Header

parent ∷ ∀ q m. H.Component q Header Header m
parent = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval {
      handleAction = handleAction
    }
    initialState = identity
    render (Header { collapsed, title, children }) = case collapsed of
      true  → HH.li [HE.onClick (const Toggle)] [HH.text title]
      false → HH.li [HE.onClick (const Toggle)] [HH.text title, renderMenu children]
    toggle  (Header α) = Header $ α { collapsed = not α.collapsed }
    update' (Header c) (Header α) = Header $ case α.children of
      MenuTracks _ → α
      MenuParent ω → α { 
        children = MenuParent $ update (pure ∘ Header ∘ const c) c.title ω 
      }
    handleAction α = case α of
      Toggle   → H.modify toggle >>= H.raise
      Update ω → H.modify (update' ω) >>= H.raise

_header = Proxy ∷ Proxy "header"
type HeaderSlot = ∀ q. H.Slot q Header Unit

_tracks = Proxy ∷ Proxy "tracks"
type TracksSlot = ∀ q. H.Slot q Void Unit

type Slots = (header ∷ HeaderSlot, tracks ∷ TracksSlot)

renderMenu ∷ ∀ m. Menu → H.ComponentHTML ParentAction Slots m
renderMenu (MenuTracks α) = HH.slot_ _tracks unit tracks α
renderMenu (MenuParent α) = HH.ul_ $ fromFoldable $ map (\x → HH.slot _header unit parent x Update) (values α)

topMenu ∷ ∀ q m. H.Component q Menu Menu m
topMenu = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState = identity
    render = renderMenu
    -- NEED TO HANDLE ACTIONS: handleAction (Update α) = H.raise 


--renderLibView ∷ ∀ a m. LibView → H.ComponentHTML a Slots m
--renderLibView (Head α) = HH.slot_ _libHead unit libHead α
--renderLibView (Leaf α) = HH.slot_ _libLeaf unit libLeaf α
--renderLibView (Node α) = HH.slot_ _libNode unit libNode α

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
