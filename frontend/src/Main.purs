module Main where

import Prelude (bind)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Components.App (app)

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI app unit body
