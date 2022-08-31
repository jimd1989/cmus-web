module Helpers ((◀), (◁), head', note, readInt, readInt') where

import Prelude (Int, (.), fmap, pure)
import Control.Arrow ((+++), (|||))
import Control.Monad (Monad, (<=<))
import Control.Monad.Except (MonadError, liftEither, throwError)
import Data.Function (const)
import Data.Functor (Functor)
import Data.Maybe (Maybe, maybe)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Tuple (fst)
import Network.HTTP.Types.Status (Status, status500)

readInt ∷ MonadError Status m ⇒ Text → m Int
readInt = liftEither . (const status500 +++ fst) . decimal

readInt' ∷ Int → Text → Int
readInt' n = (const n ||| fst) . decimal

head' ∷ MonadError Status m ⇒ [a] → m a
head' []      = throwError status500
head' (α : _) = pure α

note ∷ (MonadError Status m) ⇒ Maybe a → m a
note = maybe (throwError status500) pure

-- Digraph Tl
(◁) ∷  Functor f ⇒ (a → b) → (c → f a) → c → f b
f ◁ g = fmap f . g
infixr 9 ◁

-- Digraph PL
(◀) ∷ Monad m ⇒ (b → m c) → (a → m b) → a → m c
f ◀ g = f <=< g
infixr 1 ◀
