module CmusRepository (add, getQueue, play, remove, sync, volume) where

-- The bridge between the server routes and cmus. Parses user input and cmus
-- output to manage player state.
--
import Prelude (String, (.), ($), (-), (*>), (<$>), (>>=), (=<<), (==), pure)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Models (Cmus(..), Track(..))
import Parse (parseTracks)
import Transformers (makeLibrary, setQueue)

-- Helpers
readVar ∷ MonadIO m ⇒ TVar a → m a
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
getQueue ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m (Vector Track)
getQueue α = do
  cmus     ← readVar α
  newCmus  ← updateQueue cmus
  writeVar α newCmus
  pure     $ queue newCmus

sync ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → m Cmus
sync α = do
  library  ← makeLibrary <$> readLibrary
  newCmus  ← updateQueue library
  writeVar α newCmus
  pure       newCmus

add ∷ (MonadError Status m, MonadIO m) ⇒ TVar Cmus → Text → m (Vector Track)
add α ns = do
  nums      ← readInts ns
  cmus      ← readVar α
  filenames ← traverse (note . filename ◁ ((library cmus) !?)) nums
  traverse_ addQueue filenames
  newCmus   ← updateQueue cmus
  writeVar  α newCmus
  pure      $ queue newCmus

remove ∷ (MonadError Status m, MonadIO m) ⇒ 
         TVar Cmus → Text → Text → m (Vector Track)
remove α n m = do
  appQueueLen ← readInt n
  num         ← readInt m
  currentLen  ← queueLength
  case (currentLen == appQueueLen) of
    False → getQueue α
    True  → viewQueue *> doNTimes moveDown num *> deleteTrack *> getQueue α
  
play ∷ MonadIO m ⇒ m ()
play = playPause

volume ∷ (MonadError Status m, MonadIO m) ⇒ Text → m Int
volume n = writeVolume n *> readVolume
