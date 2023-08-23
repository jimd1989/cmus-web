module Helpers where

import Prelude ((<=<), (<<<), compose, map)
import Control.Applicative (class Applicative)
import Control.Apply (apply)
import Control.Bind (class Bind, composeKleisliFlipped)
import Data.Bitraversable (bisequence)
import Data.Functor (class Functor, mapFlipped)
import Data.Ord (lessThanOrEq, greaterThanOrEq)
import Data.Profunctor.Strong (fanout)
import Data.Semigroup (append)
import Data.Tuple (uncurry)

blackbird :: ∀ a b c d. (c → d) → (a → b → c) → a → b → d
blackbird = (<<<)<<<(<<<)
infixr 8 blackbird as ...

mapCompose ∷ ∀ a b c f. Functor f ⇒ (a → b) → (c → f a) → c → f b
mapCompose f = compose (map f)

liftFork ∷ ∀ a b c d f.
  Bind f ⇒ Applicative f ⇒ (b → c → f d) → (a → f b) → (a → f c) → a → f d
liftFork f g h = uncurry f <=< bisequence <<< fanout g h

-- Digraph Dw
infixr 5 append as ◇

-- Digraph Ob
infixr 9 compose as ∘

-- Digraph Tl = ◁
infixr 9 mapCompose as ◁

-- Digraph PL = ◀
infixr 1 composeKleisliFlipped as ◀

-- Digraph 0.
infixl 4 map as ⊙

-- dig mr 8854
infixl 1 mapFlipped as ⊖

-- Digraph 0M
infixl 4 apply as ●

-- Digraph =<
infixl 4 lessThanOrEq as ≤

-- Digraph >=
infixl 4 greaterThanOrEq as ≥
