module Models.Tracks (Tracks(..), tracks) where

import Prelude (($), (=<<), map)
import Data.List (List(..), (:))
import Data.Array (groupBy, head)
import Data.Array.NonEmpty (toArray)
import Data.Eq (eq)
import Data.Function (on)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Helpers ((∘))
import Models.Track (Track)

type Tag = Track → Maybe String

data Tracks = Head (Array Tracks)
            | Leaf (Array Track)
            | Node (Maybe String) (Array Tracks)

tracks ∷ NonEmpty List Tag → Array Track → Tracks
tracks α ω = Head $ children α ω

children ∷ NonEmpty List Tag → Array Track → Array Tracks
children (tag :| α) ω = map (tracks' α ∘ toArray) $ groupBy (on eq tag) ω

tracks' ∷ List (Track → Maybe String) → Array Track → Tracks
tracks'       Nil ω = Leaf ω
tracks' (tag : α) ω = Node (tag =<< head ω) (children (tag :| α) ω)
