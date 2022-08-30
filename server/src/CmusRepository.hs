module CmusRepository where

import Prelude (Int, String, (.), ($), (-), (*>), (<$>), (>>=), pure)
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

-- Helpers
readVar ∷ MonadIO m ⇒ TVar a → m a
readVar = liftIO . readTVarIO

writeVar ∷ MonadIO m ⇒ TVar a → a → m ()
writeVar α = liftIO . atomically . writeTVar α

readInt ∷ MonadError Status m ⇒ Text → m Int
readInt = liftEither . (const status500 +++ fst) . decimal

readInts ∷ MonadError Status m ⇒ Text → m [Int]
readInts = traverse readInt . splitOn "-"

readCmus ∷ MonadIO m ⇒ [String] → m Text
readCmus α = liftIO $ pack <$> readProcess "cmus-remote" α ""

readQueue ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readQueue = readCmus ["-C", "save -q -e -"] >>= parseCmus

addQueue ∷ MonadIO m ⇒ Text → m ()
addQueue α = readCmus ["-q", unpack α] $> ()

readLibrary ∷ (MonadError Status m, MonadIO m) ⇒ m [InputTrack]
readLibrary = readCmus ["-C", "save -l -e -"] >>= parseCmus

viewQueue ∷ MonadIO m ⇒ m ()
viewQueue = readCmus ["-C", "view 4"] *> readCmus ["-C", "win-top"] $> ()

moveDown ∷ MonadIO m ⇒ m ()
moveDown = readCmus ["-C", "win-down"] $> ()

deleteTrack ∷ MonadIO m ⇒ m ()
deleteTrack = readCmus ["-C", "win-remove"] $> ()

doNTimes ∷ MonadIO m ⇒ m () → Int → m ()
doNTimes _ 0 = pure ()
doNTimes f n = traverse_ (const f) [0 .. (n - 1)]

playPause ∷ MonadIO m ⇒ m ()
playPause = readCmus ["-u"] $> ()

updateQueue ∷ (MonadError Status m, MonadIO m) ⇒ Cmus → m Cmus
updateQueue α = readQueue >>= transformQueue α

-- Exposed functions
getQueue ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m [QueuedTrack]
getQueue α = do
  cmus     ← readVar α
  newCmus  ← updateQueue cmus
  writeVar α newCmus
  pure     $ queue newCmus

sync ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m Cmus
sync α = do
  library  ← transformLibrary <$> readLibrary
  newCmus  ← updateQueue library
  writeVar α newCmus
  pure       newCmus

add ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → Text → m ()
add α ns = do
  nums      ← readInts ns
  cmus      ← readVar α
  filenums  ← traverse (note . ((files cmus) !?)) nums
  traverse_ addQueue filenums
  newCmus   ← updateQueue cmus
  writeVar  α newCmus

remove ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → Text → m [QueuedTrack]
remove α n = do
  num ← readInt n
  viewQueue
  doNTimes moveDown num
  deleteTrack
  getQueue α

play ∷ MonadIO m ⇒ m ()
play = playPause
