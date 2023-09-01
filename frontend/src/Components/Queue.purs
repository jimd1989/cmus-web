module Components.Queue (queue, _queue) where

import Prelude (($), map)
import Data.Array ((..), length, zipWith)
import Data.Function (const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Helpers ((∘))
import Models (Track)
import Network (getDel, getQueue, handleNet)

_queue = Proxy ∷ Proxy "queue"

data Action = Init | Del Int Int

type QueueState = { 
  err ∷ Maybe String, 
  tracks ∷ Array Track 
}

emptyQueueState ∷ QueueState
emptyQueueState = {
  err: Nothing,
  tracks: []
}

queue ∷ ∀ q i o m. MonadAff m ⇒ H.Component q i o m
queue = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval {
      handleAction = handleAction,
      initialize = Just Init
    }
    initialState = const emptyQueueState
    render { tracks } = ul tracks
    catch ω = H.modify_ _ {err = Just ω}
    update ω = H.modify_ _ {tracks = ω}
    handleAction α = case α of
      Init    → handleNet catch update getQueue
      Del n m → handleNet catch update (getDel n m)

p ∷ ∀ w i. Array (Maybe String) → HH.HTML w i
p = HH.text ∘ joinWith " " ∘ map (fromMaybe "")

li ∷ ∀ w. Int → Int → Track → HH.HTML w Action
li n m α = HH.li [HE.onClick (const $ Del n m)] [
         HH.div [HP.classes [HH.ClassName "queued-track"]] [
           HH.div [HP.classes [HH.ClassName "artist-track"]] [
             HH.div [HP.classes [HH.ClassName "artist-track-contents"]] [
               HH.div_ [p [α.artist]],
               HH.div_ [p [α.title]]
             ]
           ],
           HH.div [HP.classes [HH.ClassName "time"]] [p [α.len]]
         ]
       ]

ul ∷ ∀ w. Array Track → HH.HTML w Action
ul α = HH.ul [HP.id "queue"] (zipWith (li (length α)) (0 .. (length α)) α)
