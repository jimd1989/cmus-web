module Components.App where

import Prelude (($))
import Data.Function (const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helpers ((◇))
import Network (getPausePlay, handleNet)

data Screen = Err String | Ok String | Library | Queue
data Action = Play

type AppState = {
  vol ∷ Int,
  screen ∷ Screen
}

emptyAppState ∷ AppState 
emptyAppState = {
  vol: 0,
  screen: Library
}

app ∷ ∀ q o i m. MonadAff m ⇒ H.Component q i o m
app = H.mkComponent { initialState, render, eval }
  where 
  eval = H.mkEval H.defaultEval {
      handleAction = handleAction
    }
  initialState = const emptyAppState
  renderScreen (Err α) = HH.text ("ERROR " ◇ α)
  renderScreen (Ok  α) = HH.text ("OK " ◇ α)
  renderScreen _         = HH.button [HE.onClick (const $ Play)] [HH.text "a"]
  render { vol, screen } = renderScreen screen
  handleAction α = case α of
    Play → handleNet (\ω → H.modify_ _{screen = Err ω}) (\ω → H.modify_ _{screen = Ok ω}) getPausePlay
      
  
