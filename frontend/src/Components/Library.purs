module Components.Library (library, _library) where

import Prelude (show)
import Data.Array (length)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Helpers ((◇))
import Models.LibView (libView)
import Models.Track (Track)

_library = Proxy ∷ Proxy "library"

type LibraryState = { tracks ∷ Array Track }

library ∷ ∀ q o m. H.Component q (Array Track) o m
library = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState α = { tracks: α }
    render { tracks } = HH.text ((show (length tracks)) ◇ " tracks")
