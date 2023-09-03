module Repositories.Cmus (add, getQueue, play, remove, skip, sync, volume) where

-- The bridge between the server routes and cmus. Parses user input and cmus
-- output to manage player state.
--
import Prelude (String, (.), ($), (-), (*>), (<$>), (>>=), (=<<), (==), pure)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Foldable (traverse_)
import Data.Bool (Bool(..))
import Data.Function (const)
import Data.Functor (($>))
import Data.Int (Int)
import Data.List (drop)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Traversable (traverse)
import Data.Vector (Vector, (!?))
import GHC.Conc (TVar, atomically, readTVarIO, writeTVar)
import Network.HTTP.Types.Status (Status)
import System.Process (readProcess)
import Helpers ((◁), (◀), head', note, readInt)
import Models.Config (Config(..))
import Models.Cmus (Cmus(..))
import Models.Track (Track(..))
import Parse (parseTracks)
import Transformers (makeLibrary, setQueue)

-- Helpers
cmusVar ∷ MonadReader Config m ⇒ m (TVar Cmus)
cmusVar = cmus <$> ask

readVar ∷ (MonadIO m) ⇒ TVar a → m a
readVar = liftIO . readTVarIO

writeVar ∷ MonadIO m ⇒ TVar a → a → m ()
writeVar α = liftIO . atomically . writeTVar α

readInts ∷ MonadError Status m ⇒ Text → m [Int]
readInts = traverse readInt . splitOn "-"

readCmus ∷ MonadIO m ⇒ [String] → m Text
readCmus α = liftIO $ pack <$> readProcess "cmus-remote" α ""

readQueue ∷ (MonadError Status m, MonadIO m) ⇒ m [Track]
readQueue = readCmus ["-C", "save -q -e -"] >>= parseTracks

queueLength ∷ (MonadError Status m, MonadIO m) ⇒ m Int
queueLength = (readInt ◀ pack ◁ bc ◀ wc ◀ grep ◀ cut) =<< query 
  where query = liftIO $ readProcess "cmus-remote" ["-C", "save -q -e -"] ""
        cut   = liftIO . readProcess "cut" ["-d", " ", "-f", "1"]
        grep  = liftIO . readProcess "grep" ["file"]
        wc    = liftIO . readProcess "wc" ["-l"]
        bc    = liftIO . readProcess "bc" []

addQueue ∷ MonadIO m ⇒ Text → m ()
addQueue α = readCmus ["-q", unpack α] $> ()

readLibrary ∷ (MonadError Status m, MonadIO m) ⇒ m [Track]
readLibrary = readCmus ["-C", "save -l -e -"] >>= parseTracks

readVolume ∷ (MonadError Status m, MonadIO m) ⇒ m Int
readVolume = (parse ◀ split ◁ grep) =<< query
  where query = liftIO $ readProcess "cmus-remote" ["-Q"] ""
        grep  = liftIO . readProcess "grep" ["vol_left"]
        split = drop 2 . splitOn " " . pack
        parse = readInt ◀ (head' . splitOn "\n") ◀ head'

writeVolume ∷ MonadIO m ⇒ Text → m ()
writeVolume n = liftIO $ readCmus ["-v", unpack n] $> ()

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
updateQueue α = readQueue >>= setQueue α

-- Exposed functions
getQueue ∷ (MonadReader Config m, MonadError Status m, MonadIO m) ⇒ 
           m (Vector Track)
getQueue = do
  cmusPointer ← cmusVar
  playerState ← readVar cmusPointer
  newState    ← updateQueue playerState
  writeVar cmusPointer newState
  pure        $ queue newState

sync ∷ (MonadReader Config m, MonadError Status m, MonadIO m) ⇒  m Cmus
sync = do
  syncedPlayer ← makeLibrary <$> readLibrary
  cmusPointer  ← cmusVar
  writeVar cmusPointer syncedPlayer
  pure syncedPlayer

add ∷ (MonadReader Config m, MonadError Status m, MonadIO m) ⇒
      Text → m (Vector Track)
add ns = do
  nums        ← readInts ns
  cmusPointer ← cmusVar
  playerState ← readVar cmusPointer
  filenames   ← traverse (note . filename ◁ ((library playerState) !?)) nums
  traverse_ addQueue filenames
  newState    ← updateQueue playerState
  writeVar cmusPointer newState
  pure        $ queue newState

remove ∷ (MonadReader Config m, MonadError Status m, MonadIO m) ⇒ 
         Text → Text → m (Vector Track)
remove n m = do
  appQueueLen ← readInt n
  num         ← readInt m
  currentLen  ← queueLength
  case (currentLen == appQueueLen) of
    False → getQueue
    True  → viewQueue *> doNTimes moveDown num *> deleteTrack *> getQueue
  
play ∷ MonadIO m ⇒ m ()
play = playPause

skip ∷ MonadIO m ⇒ m ()
skip = readCmus ["-q", "-n"] $> ()

volume ∷ (MonadError Status m, MonadIO m) ⇒ Text → m Int
volume n = writeVolume n *> readVolume
