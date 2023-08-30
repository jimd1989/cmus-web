module Network where

import Prelude (($), (>>=), bind, show, pure)
import Affjax.ResponseFormat (string)
import Affjax.Web (Error, get, printError)
import Control.Monad.Error.Class (class MonadError, liftEither)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either)
import Data.Function (const)
import Data.Functor (($>))
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Choice ((+++), (|||))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (href)
import Helpers ((∘), (◇), (⊙), (◀))
import Models (Cmus, Track, parseCmus, parseTracks)

getAppUrl ∷ ∀ m. MonadEffect m ⇒ m String
getAppUrl = liftEffect $ window >>= location >>= href

body ∷ ∀ m r. MonadError String m ⇒ Either Error {body ∷ String | r} → m String
body = liftEither ∘ (printError +++ _.body)

getString ∷ ∀ m. MonadError String m ⇒ MonadAff m ⇒ String → m String
getString path = do
  url  ← getAppUrl
  resp ← liftAff $ body ⊙ (get string (url ◇ path))
  resp

getObj ∷ ∀ m a. MonadError String m ⇒ MonadAff m ⇒ (String → m a) → String → m a
getObj parse path = getString path >>= parse

getCmus ∷ ∀ m. MonadError String m ⇒ MonadAff m ⇒ m Cmus
getCmus = getObj parseCmus "sync"

getQueue ∷ ∀ m. MonadError String m ⇒ MonadAff m ⇒ m (Array Track)
getQueue = getObj parseTracks "queue"

getVolume ∷ ∀ m. MonadError String m ⇒ MonadAff m ⇒ Int → m Int
getVolume n = parseInt ⊙ getString path
  where path     = "vol/" ◇ (show n)
        parseInt = fromMaybe 0 ∘ fromString

getPausePlay ∷ ∀ m. MonadError String m ⇒ MonadAff m ⇒ m String
getPausePlay = getString "play"

-- Provide two DOM-altering functions to handle error/success cases of a
-- network request
handleNet ∷ ∀ m a b. 
            MonadAff m ⇒ (String → m b) → (a → m b) → ExceptT String m a → m b
handleNet l r = (l ||| r) ◀ runExceptT
