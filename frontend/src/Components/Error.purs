module Components.Error (error, _error) where

import Data.Function (const, identity)
import Data.Unit (Unit, unit)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

_error = Proxy ∷ Proxy "error"

data Action = Close

error ∷ ∀ q m. H.Component q String Unit m
error = H.mkComponent { initialState, render, eval }
  where
    eval = H.mkEval H.defaultEval {
      handleAction = handleAction
    }
    initialState = identity
    render message = HH.div [HP.id "error-box"] [
      HH.div [HP.id "error-close"] [
        HH.button [HE.onClick (const Close)] [HH.text "x"]
      ],
      HH.div [HP.id "error-message"] [HH.text message]
    ]
    handleAction Close = H.raise unit
