module Types (App(..), Config(..), config, runApp) where

import Prelude
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality as TE
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (href)
import Helpers ((⊙))

newtype App a = App (ReaderT Config Aff a)
derive instance newtypeApp ∷ Newtype (App a) _
derive newtype instance functorApp ∷ Functor App
derive newtype instance applyApp ∷ Apply App
derive newtype instance applicativeApp :: Applicative App
derive newtype instance bindApp :: Bind App
derive newtype instance monadApp :: Monad App
derive newtype instance monadEffectApp :: MonadEffect App
derive newtype instance monadAffApp :: MonadAff App

instance monadAskApp ∷ TE.TypeEquals c Config ⇒ MonadAsk c App where
  ask = App $ asks TE.from

runApp ∷ ∀ a. App a → Config → Aff a
runApp m conf = runReaderT (unwrap m) conf

type Config = { url ∷ String }

config ∷ ∀ m. MonadEffect m ⇒ m Config
config = { url: _ } ⊙ getAppUrl

getAppUrl ∷ ∀ m. MonadEffect m ⇒ m String
getAppUrl = liftEffect $ window >>= location >>= href
