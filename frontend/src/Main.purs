module Main where

import Prelude ((>), (<), (+), (-), ($), (>>=), bind, show)
import Affjax.ResponseFormat (string)
import Affjax.Web (get)
import Data.Either (hush)
import Data.Function (const)
import Data.Functor (($>))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (href)
import Helpers ((◇), (⊙), (∘), (◁), (◀))

type URL = String
data Action = Initialize
            | Play URL
            | Vol  URL Int

c ∷ ∀ q o i m. MonadAff m ⇒ MonadEffect m ⇒ H.Component q i o m
c = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval { 
      handleAction = handleAction,
      initialize = Just Initialize
    }
    initialState = const {url: "", vol: 0}
    render { url, vol } = HH.div [HP.id "menu"] [
      HH.button [] [HH.text "⌂"],
      HH.button [] [HH.text "≡"],
      HH.button [HE.onClick (const $ Vol url (bounds $ vol - 5))] [HH.text "-"],
      HH.button [HE.onClick (const $ Play url)] [HH.text "▶"],
      HH.button [HE.onClick (const $ Vol url (bounds $ vol + 5))] [HH.text "+"],
      HH.span [HP.id "vol"] [HH.text (show vol ◇ "%")]
    ]
    handleAction α = case α of
      (Play ω  ) → H.liftAff $ pausePlay ω
      (Vol  ω n) → do
        newVol ← H.liftAff $ setVolume ω n
        H.modify_ _ {vol = newVol}
      Initialize → do
        url    ← H.liftEffect getAppUrl
        newVol ← setVolume url 100
        H.modify_ _ {url = url, vol = newVol}

getAppUrl ∷ ∀ m. MonadEffect m ⇒ m String
getAppUrl = liftEffect $ window >>= location >>= href

pausePlay ∷ ∀ m. MonadAff m ⇒ URL → m Unit
pausePlay α = liftAff $ get string (α ◇ "play") $> unit

setVolume ∷ ∀ m. MonadAff m ⇒ URL → Int → m Int
setVolume α n = (fromMaybe 0 ∘ (fromString ◀ _.body ◁ hush)) ⊙ req
  where req = liftAff $ get string (α ◇ "vol/" ◇ show n)

bounds ∷ Int → Int
bounds n | n < 0   = 0
bounds n | n > 100 = 100
bounds n           = n

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI c unit body
