module Components.Library (library, _library) where

import Prelude (map)
import Data.Array (length)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Models.LibView (LibHead(..), LibLeaf, LibNode(..), LibView(..), libView)
import Models.Track (Track)

_library = Proxy ∷ Proxy "library"

type LibraryState = { 
  lib ∷ Maybe LibHead,
  err ∷ Maybe String
}

order ∷ NonEmpty List (Track → Maybe String)
order = (_.albumArtist) :| (_.album) : Nil

library ∷ ∀ q o m. H.Component q (Array Track) o m
library = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState α = case (libView order α) of
      Head ω → { lib: Just ω,  err: Nothing }
      _      → { lib: Nothing, err: Just "Library render failed" }
    render { err, lib } = case (Tuple err lib) of
      (Tuple (Just α) _) → HH.text α
      (Tuple _ (Just (LibHead { children }))) → HH.div_ (map r children)
      (Tuple _ _) → HH.text "Library"

r ∷ ∀ w i. LibView → HH.HTML w i
r (Node α) = renderNode α
r _ = HH.p_ [HH.text ""]

renderNode ∷ ∀ w i. LibNode → HH.HTML w i
renderNode (LibNode { children, header }) = HH.p_ [HH.text (g header)]
  where g = fromMaybe ""
