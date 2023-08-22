module Main where

import Prelude
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (href)

hello ∷ Effect Unit
hello = log "hello"

data Action = Initialize | Hello

c ∷ ∀ q o i m. MonadEffect m ⇒ H.Component q i o m
c = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval { 
      handleAction = handleAction,
      initialize = Just Initialize
    }
    initialState = const {url: ""}
    render { url } = HH.div_ [
      HH.text url,
      HH.button [HE.onClick (const Hello)] [HH.text "!"]
    ]
    handleAction Initialize = H.liftEffect appUrl >>= \url → H.modify_ (const { url })
    handleAction Hello = H.liftEffect hello

appUrl ∷ ∀ m. MonadEffect m ⇒ m String
appUrl = liftEffect $ window >>= location >>= href

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI c unit body
