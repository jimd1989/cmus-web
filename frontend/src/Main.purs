module Main where

import Prelude
import Data.Function (const)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

hello ∷ Effect Unit
hello = log "hello"

data Action = Hello

c ∷ ∀ q o i m. MonadEffect m ⇒ H.Component q i o m
c = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval { handleAction = handleAction }
    initialState = const 0
    render α = HH.div_ [
      HH.text (show α),
      HH.button [HE.onClick (const Hello)] [HH.text "!"]
    ]
    handleAction Hello = H.liftEffect hello

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI c unit body
