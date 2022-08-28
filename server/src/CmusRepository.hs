module CmusRepository where

import Prelude (Int, String, (.), ($), (<$>), (>>=), pure)
import Control.Arrow ((+++))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (const)
import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Data.Text.Read (decimal)
import Data.Tuple (fst)
import Data.Vector ((!?))
import GHC.Conc (TVar, atomically, readTVarIO, writeTVar)
import Network.HTTP.Types.Status (Status, status500)
import System.Process (readProcess)
import Models (Cmus(..), InputTrack)
import Parse (parseCmus)
import Transformers (note, transformLibrary, transformQueue)

readVar ∷ MonadIO m ⇒ TVar a → m a
readVar = liftIO . readTVarIO

writeVar ∷ MonadIO m ⇒ TVar a → a → m ()
writeVar α = liftIO . atomically . writeTVar α

readInt ∷ MonadError Status m ⇒ Text → m Int
readInt = liftEither . (const status500 +++ fst) . decimal

readCmus ∷ MonadIO m ⇒ [String] → m Text
readCmus α = liftIO $ pack <$> readProcess "cmus-remote" α ""

readQueue ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readQueue = readCmus ["-C", "save -q -e -"] >>= parseCmus

addQueue ∷ MonadIO m ⇒ Text → m ()
addQueue α = readCmus ["-q", unpack α] $> ()

readLibrary ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readLibrary = readCmus ["-C", "save -l -e -"] >>= parseCmus

add ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → Text → m ()
add α n = do
  num ← readInt n
  cmus ← readVar α
  file ← note $ (files cmus) !? num
  addQueue file

sync ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m Cmus
sync α = do
  library ← transformLibrary <$> readLibrary
  newCmus ← readQueue >>= transformQueue library
  writeVar α newCmus
  pure newCmus
