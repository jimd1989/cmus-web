module Transformers (makeLibrary, setQueue) where

import Prelude (Int, ($), (.), (=<<), div, flip, length, mod, show, traverse)
import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError) 
import Data.Eq ((==))
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.Maybe (Maybe)
import Data.Text (Text, pack, unpack)
import Data.Tuple (uncurry)
import qualified Data.Vector as V
import Network.HTTP.Types.Status (Status)
import Text.Read (readMaybe)
import Helpers ((◀), (◁), (◇), note)
import Models.Cmus (Cmus(..))
import Models.Track (Track(..))

-- Concered with refining raw Track records into more more advanced Cmus record
-- state, which is returned to the frontend.

numberTracks ∷ [Track] → [Track]
numberTracks = L.zipWith enumerate [0 ..]
  where enumerate n ω = ω { key = n, duration' = toMinutes =<< duration ω } 

toMinutes ∷ Text → Maybe Text
toMinutes = (pack . format) ◁ readMaybe . unpack
      where format ω = (show $ ω `div` 60) ◇ ":" ◇ (zero $ show $ ω `mod` 60)
            zero β | (length β) == 1 = "0" ◇ β
            zero β                   = β 

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
        
