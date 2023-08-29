module Transformers (makeLibrary, setQueue) where

import Prelude (Int, (.), flip, traverse)
import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError) 
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.Text (Text)
import Data.Tuple (uncurry)
import qualified Data.Vector as V
import Network.HTTP.Types.Status (Status)
import Helpers ((◀), (◁), note)
import Models (Cmus(..), Track(..))

-- Concered with refining raw Track records into more more advanced Cmus record
-- state, which is returned to the frontend.

numberTracks ∷ [Track] → [Track]
numberTracks = L.zipWith (\n ω → ω { key = n }) [0 ..]

filesToIndices ∷ [Track] → H.HashMap Text Int
filesToIndices = H.fromList . L.map (filename &&& key)

makeLibrary ∷ [Track] → Cmus
makeLibrary = uncurry (Cmus V.empty) . filesAndLibrary
  where filesAndLibrary = (filesToIndices &&& V.fromList) . numberTracks

setQueue ∷ MonadError Status m ⇒ Cmus → [Track] → m Cmus
setQueue α = newQueue ◁ V.fromList ◁ traverse libLook ◀ traverse filesLook
  where filesLook  = note . (flip H.lookup (files α)) . filename
        libLook    = note . ((library α) V.!?)
        newQueue ω = α { queue = ω }
        
