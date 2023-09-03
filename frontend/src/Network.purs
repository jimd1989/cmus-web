module Network (getCmus, getDel, getPlay, getVol, getQueue, handleNet) where

import Prelude (($), (>>=), bind, show)
import Affjax.ResponseFormat (string)
import Affjax.Web (Error, get, printError)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Error.Class (class MonadError, liftEither)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Choice ((+++), (|||))
import Effect.Aff.Class (class MonadAff, liftAff)
import Helpers ((∘), (◇), (⊙), (◀))
import Models.Cmus (Cmus, parseCmus)
import Models.Track (Track, parseTracks)
import Types (Config)

body ∷ ∀ m r. MonadError String m ⇒ Either Error {body ∷ String | r} → m String
body = liftEither ∘ (printError +++ _.body)

getString ∷ ∀ m. MonadAsk Config m ⇒ MonadError String m ⇒ MonadAff m ⇒ 
            String → m String
getString path = do
  u    ← ask
  resp ← liftAff $ body ⊙ (get string (u.url ◇ path))
  resp

getObj ∷ ∀ m a. MonadAsk Config m ⇒ MonadError String m ⇒ MonadAff m ⇒ 
         (String → m a) → String → m a
getObj parse path = getString path >>= parse

getCmus ∷ ∀ m. MonadAsk Config m ⇒ MonadError String m ⇒ MonadAff m ⇒ m Cmus
getCmus = getObj parseCmus "sync"

getDel ∷ ∀ m. MonadAsk Config m ⇒ MonadError String m ⇒ MonadAff m ⇒ 
         Int → Int → m (Array Track)
getDel n m = getObj parseTracks ("remove/" ◇ (show n) ◇ "/" ◇ (show m))

getQueue ∷ ∀ m. MonadAsk Config m ⇒ MonadError String m ⇒ 
           MonadAff m ⇒ m (Array Track)
getQueue = getObj parseTracks "queue"

getVol ∷ ∀ m. MonadAsk Config m ⇒ MonadError String m ⇒ MonadAff m ⇒ 
         Int → m Int
getVol n = parseInt ⊙ getString path
  where path     = "vol/" ◇ (show n)
        parseInt = fromMaybe 0 ∘ fromString

getPlay ∷ ∀ m. MonadAsk Config m ⇒ MonadError String m ⇒ MonadAff m ⇒ m String
getPlay = getString "play"

-- Unwrap the Either from a network request and provide two component state
-- altering functions to handle failure/success cases respectively.
handleNet ∷ ∀ m a b. MonadAff m ⇒ 
            (String → m b) → (a → m b) → ExceptT String m a → m b
handleNet l r = (l ||| r) ◀ runExceptT
