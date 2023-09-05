module Models.SubMenu where

import Prelude (($))
import Data.Array (groupBy, head)
import Data.Array.NonEmpty (toArray)
import Data.Eq (eq)
import Data.Function (on)
import Data.List (List, (:))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..))
import Helpers ((◁), (◀))
import Models.Track (TagLens, Track)


type Library = Map String SubMenu
data SubMenu = MenuHeader (Map String Header) | MenuContents Contents

newtype Header = Header { 
  collapsed ∷ Boolean,
  title ∷ String, 
  children ∷ SubMenu
}

newtype Contents = Contents {
  contents ∷ Array Track
}

contents ∷ Array Track → Contents
contents α = Contents { contents: α }

header ∷ Maybe String → SubMenu → Header
header t children = Header { collapsed: true, title, children }
  where title = fromMaybe "" t

lib ∷ NonEmpty List TagLens → Array Track → Library
lib α ω = fromFoldable [(Tuple "ass" (MenuContents $ contents []))]

-- WIP
--withTag ∷ TagLens → Array Track → Array (Tuple String (Array Track))
--withTag (t :| α) = (f ∘ toArray) ◁ groupBy (on eq t)
--  where f = (fromMaybe "" ∘ t ◀ head) &&&  
