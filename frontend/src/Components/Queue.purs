module Components.Queue (queue, _queue, li) where

import Prelude (map)
import Data.Maybe (Maybe, fromMaybe)
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Helpers ((∘))
import Models (Track)

_queue = Proxy ∷ Proxy "queue"

type QueueState = { tracks ∷ Array Track }

queue ∷ ∀ q o m. H.Component q (Array Track) o m
queue = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval
    initialState α = { tracks: α }
    render { tracks } = ul tracks

p ∷ ∀ w i. Array (Maybe String) → HH.HTML w i
p = HH.text ∘ joinWith " " ∘ map (fromMaybe "")

li ∷ ∀ w i. Track → HH.HTML w i
li α = HH.li [HP.classes [HH.ClassName "queued-track"]] [
         HH.div_ [p [α.artist]],
         HH.div_ [p [α.trackN, α.title]],
         HH.div_ [p [α.year, α.len]]
       ]

ul ∷ ∀ w i. Array Track → HH.HTML w i
ul α = HH.ul [HP.id "queue"] (map li α)
