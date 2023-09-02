module Models.Cmus (Cmus(..), cmus') where

import Prelude (Int)
import Data.Aeson (ToJSON(..), (.=), object)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Vector as V
import Models.Track (Track)

data Cmus = Cmus {
  queue ∷ V.Vector Track,     -- Tracks currently queued for listening
  files ∷ H.HashMap Text Int, -- Filename → Track key mapping; for queue reading
  library ∷ V.Vector Track    -- All Tracks available for listening
}

cmus' ∷ Cmus
cmus' = Cmus V.empty H.empty V.empty

instance ToJSON Cmus where
  toJSON α = object [ "library" .= library α, "queue" .= queue α ]
