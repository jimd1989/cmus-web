module CmusRepository where

import Prelude (Int, String, (.), ($), (<$>), (>>=), pure)
import Control.Arrow ((+++))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Function (const)
import Data.Functor (($>))
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Read (decimal)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Vector ((!?))
import GHC.Conc (TVar, atomically, readTVarIO, writeTVar)
import Network.HTTP.Types.Status (Status, status500)
import System.Process (readProcess)
import Models (Cmus(..), InputTrack, QueuedTrack)
import Parse (parseCmus)
import Transformers (note, transformLibrary, transformQueue)

readVar ∷ MonadIO m ⇒ TVar a → m a
readVar = liftIO . readTVarIO

writeVar ∷ MonadIO m ⇒ TVar a → a → m ()
writeVar α = liftIO . atomically . writeTVar α

readInts ∷ MonadError Status m ⇒ Text → m [Int]
readInts = traverse readInt . splitOn "-"
  where readInt = liftEither . (const status500 +++ fst) . decimal

readCmus ∷ MonadIO m ⇒ [String] → m Text
readCmus α = liftIO $ pack <$> readProcess "cmus-remote" α ""

readQueue ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readQueue = readCmus ["-C", "save -q -e -"] >>= parseCmus

addQueue ∷ MonadIO m ⇒ Text → m ()
addQueue α = readCmus ["-q", unpack α] $> ()

readLibrary ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readLibrary = readCmus ["-C", "save -l -e -"] >>= parseCmus

add ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → Text → m ()
add α ns = do
  nums      ← readInts ns
  filenames ← files <$> readVar α
  filenums  ← traverse (note . (filenames !?)) nums
  traverse_ addQueue filenums

getQueue ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m [QueuedTrack]
getQueue α = do
  cmus    ← readVar α
  newCmus ← readQueue >>= transformQueue cmus
  writeVar α newCmus
  pure $ queue newCmus

sync ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m Cmus
sync α = do
  library ← transformLibrary <$> readLibrary
  newCmus ← readQueue >>= transformQueue library
  writeVar α newCmus
  pure newCmus
