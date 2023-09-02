module Main where

import Prelude (bind, flip)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Components.App (app)
import Types (config, runApp)

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  conf ← config
  let app' = H.hoist (flip runApp conf) app
  runUI app' unit body
