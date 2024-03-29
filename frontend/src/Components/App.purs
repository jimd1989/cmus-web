module Components.App (app) where

import Prelude (($), (+), (-), (>), (<), pure, show)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Eq (class Eq, (==))
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Components.Error (error, _error)
import Components.Library (library, _library)
import Components.Queue (queue, _queue)
import Helpers ((◇))
import Models.Cmus (Cmus)
import Network (getCmus, getPlay, getSkip, getVol, handleNet)
import Types (Config)

data Screen = Loading | Library | Queue
derive instance eqScreen ∷ Eq Screen

data Action = Init | Play | Skip | Vol Int | Switch Screen | CloseErr Unit

type AppState = {
  cmus ∷ Cmus,
  err ∷ Maybe String,
  vol ∷ Int,
  screen ∷ Screen
}

emptyAppState ∷ AppState 
emptyAppState = {
  cmus: { library: [] },
  err: Nothing,
  vol: 0,
  screen: Loading
}

app ∷ ∀ q o i m. MonadAsk Config m ⇒ MonadAff m ⇒ H.Component q i o m
app = H.mkComponent { initialState, render, eval }
  where 
  eval = H.mkEval H.defaultEval {
      handleAction = handleAction,
      initialize = Just Init
    }
  initialState = const emptyAppState
  render { vol, screen, cmus, err } = HH.div [HP.id "app"] [
    HH.div [HP.id "top-menu"] [
      HH.button [HE.onClick (const $ Switch Library),
                 HP.classes [activeButton Library screen]] 
                [HH.text "⌂"],
      HH.button [HE.onClick (const $ Switch Queue),
                 HP.classes [activeButton Queue screen]] 
                [HH.text "≡"]
    ],
    renderErr err,
    HH.div [HP.id "screens"] [renderScreen screen cmus],
    HH.div [HP.id "menu"] [
      HH.button [HE.onClick (const $ Vol (bounds $ vol - 5))] 
                [HH.text "-"],
      HH.button [HE.onClick (const Play)]
                [HH.text "▶"],
      HH.button [HE.onClick (const $ Vol (bounds $ vol + 5))] 
                [HH.text "+"],
      HH.button [HE.onClick (const Skip)] 
                [HH.text "⇒"],
      HH.div [HP.id "vol"] 
             [HH.text (show vol ◇ "%")]
    ]
  ]
  renderErr Nothing  = HH.span_ []
  renderErr (Just α) = HH.slot _error unit error α CloseErr
  renderScreen α ω = case α of
    Library → HH.slot_ _library unit library (ω.library)
    Queue   → HH.slot_ _queue unit queue unit
    Loading → HH.div [HP.id "loading"] [HH.h2_ [HH.text "Connecting ..."]]
  activeButton α ω | α == ω = HH.ClassName "screen-active"
  activeButton _ _          = HH.ClassName "screen-inactive"
  noop = const (pure unit)
  catch ω = H.modify_ _ {err = Just ω}
  init ω = H.modify_ _ {cmus = ω, screen = Library}
  handleAction α = case α of
    Play       → handleNet catch noop getPlay
    Skip       → handleNet catch noop getSkip
    Vol n      → handleNet catch (\ω → H.modify_ _ {vol = ω}) (getVol n)
    Switch ω   → H.modify_ _ {screen = ω}
    Init       → handleNet catch init getCmus
    CloseErr _ → H.modify_ _ {err = Nothing}
      
bounds ∷ Int → Int
bounds n | n < 0   = 0
bounds n | n > 100 = 100
bounds n           = n
