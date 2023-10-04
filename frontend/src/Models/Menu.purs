module Models.Menu (Header(..), Menu(..), Tracks(..), menu) where

import Prelude (($))
import Data.Array (groupBy, head)
import Data.Array.NonEmpty (toArray)
import Data.Eq (eq)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Map (Map, fromFoldable)
import Data.Maybe (fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong ((&&&))
import Helpers ((◁), (◀), (∘))
import Models.Track (TagLens, Track)

data Menu = MenuParent (Map String Header) | MenuTracks Tracks

newtype Header = Header { 
  collapsed ∷ Boolean,
  title ∷ String, 
  children ∷ Menu
}

newtype Tracks = Tracks {
  contents ∷ Array Track
}

menu ∷ NonEmpty List TagLens → Array Track → Menu
menu = men

men ∷ NonEmpty List TagLens → Array Track → Menu
men (α:|β) = MenuParent ∘ fromFoldable ∘ ((title &&& rec) ∘ toArray) ◁ group
  where
    group = groupBy (on eq α)
    title = fromMaybe "" ∘ (α ◀ head)
    rec g = men' β g (title g)

men' ∷ List TagLens → Array Track → String → Header
men' Nil   ω title = Header { collapsed: true, title, children: tracks title ω }
men' (α:β) ω title = Header { collapsed: true, title, children: men (α :| β) ω }

tracks ∷ String → Array Track → Menu
tracks title contents = MenuTracks $ Tracks { contents }
