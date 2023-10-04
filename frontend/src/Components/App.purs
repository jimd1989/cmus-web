module Components.App (app) where

import Prelude (($), (+), (-), (>), (<), pure, show)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Eq (class Eq, (==))
import Data.Function (const)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Components.Error (error, _error)
import Components.Library (library, _library)
import Components.Queue (queue, _queue)
import Helpers ((◇), (∘), (⊙))
import Models.Cmus (Cmus)
import Models.Menu (Menu(..), Tracks(..), menu)
import Models.Track (TagLens)
import Network (getCmus, getPlay, getSkip, getVol, handleNet)
import Types (Config)

data Screen = Loading | Library | Queue
derive instance eqScreen ∷ Eq Screen

data Action = Init | Play | Skip | Vol Int | Switch Screen | CloseErr Unit 
            | Init' (NonEmpty List TagLens)

type AppState = {
  cmus ∷ Cmus,
  err ∷ Maybe String,
  lib ∷ Menu,
  order ∷ NonEmpty List TagLens,
  screen ∷ Screen,
  vol ∷ Int
}

emptyAppState ∷ AppState 
emptyAppState = {
  cmus: { library: [] },
  err: Nothing,
  lib: defaultLib,
  order: defaultOrder,
  screen: Loading,
  vol: 0
}

defaultOrder ∷ NonEmpty List TagLens
defaultOrder = (_.albumArtist) :| (_.album) : Nil

defaultLib ∷ Menu
defaultLib = MenuTracks $ Tracks {contents: []}

app ∷ ∀ q o i m. MonadAsk Config m ⇒ MonadAff m ⇒ H.Component q i o m
app = H.mkComponent { initialState, render, eval }
  where 
  eval = H.mkEval H.defaultEval {
      handleAction = handleAction,
      initialize = Just $ Init' defaultOrder
    }
  initialState = const emptyAppState
  render { cmus, err, screen, vol } = HH.div [HP.id "app"] [
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
  init' ω = H.modify_ _ {lib = ω, screen = Library}
  handleAction α = case α of
    Play       → handleNet catch noop getPlay
    Skip       → handleNet catch noop getSkip
    Vol n      → handleNet catch (\ω → H.modify_ _ {vol = ω}) (getVol n)
    Switch ω   → H.modify_ _ {screen = ω}
    Init       → handleNet catch init getCmus
    CloseErr _ → H.modify_ _ {err = Nothing}
    Init' ω    → handleNet catch init' ((menu ω ∘ _.library) ⊙ getCmus)
      
bounds ∷ Int → Int
bounds n | n < 0   = 0
bounds n | n > 100 = 100
bounds n           = n
