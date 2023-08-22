module LandingPage (landingPage) where

import Data.Text (Text)
import Helpers ((◇))

landingPage ∷ Text
landingPage = "<html><head><title>cmus</title></head><body>" ◇
              "<script src=\"app.js\"></script></body></html>"
