module Components.Library (library, _library) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Components.LibView (renderLibView)
import Models.LibView (LibView, libView)
import Models.Track (Track)

_library = Proxy ∷ Proxy "library"

type LibraryState = { 
  lib ∷ LibView
}

order ∷ NonEmpty List (Track → Maybe String)
order = (_.albumArtist) :| (_.album) : Nil

library ∷ ∀ q o m. H.Component q (Array Track) o m
library = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState α = { lib: libView order α }
    render { lib } = renderLibView lib
