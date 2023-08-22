module Main where

import Prelude
import Affjax.ResponseFormat (string)
import Affjax.Web (get)
import Data.Function (const)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (href)

data Action = Initialize | Play String

c ∷ ∀ q o i m. MonadAff m ⇒ MonadEffect m ⇒ H.Component q i o m
c = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval { 
      handleAction = handleAction,
      initialize = Just Initialize
    }
    initialState = const {url: ""}
    render { url } = HH.div_ [
      HH.button [HE.onClick (const $ Play url)] [HH.text "Toggle Play"]
    ]
    handleAction α = case α of
      Initialize → H.liftEffect appUrl >>= \url → H.modify_ (const { url })
      (Play ω  ) → H.liftAff $ pausePlay ω

appUrl ∷ ∀ m. MonadEffect m ⇒ m String
appUrl = liftEffect $ window >>= location >>= href

pausePlay ∷ ∀ m. MonadAff m ⇒ String → m Unit
pausePlay α = liftAff $ get string (α <> "play") $> unit

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI c unit body
