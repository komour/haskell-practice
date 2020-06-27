module CHT
  ( ConcurrentHashTable (..)
  , newCHT
  , sizeCHT
  , getCHT
  , putCHT
  ) where

import           Control.Concurrent.STM (STM, TVar, atomically, newTVar,
                                         readTVar, writeTVar)
import           Control.Monad          (forM, forM_)
import           Data.Hashable          (Hashable, hash)
import           Data.Vector            ((!))
import qualified Data.Vector            as V

-- | Naive hashtable realisation:
-- In each cell of the vector we store list
-- and in case of collisions we add new pair (key, value) to the beginning of the list.
data ConcurrentHashTable k v = ConcurrentHashTable
  { _contents :: TVar (V.Vector (TVar [(k, v)]))
  , _size     :: TVar Int
  }

-- | Initial size of any hashtable created via newCHT
initialSize :: Int
initialSize = 10

-- | Constant which indicates what percentage of filling means filled hashtable.
-- Need it to reduce the chance of collisions
fillingRate :: Float
fillingRate = 0.6

-- | Initialize an empty hashtable
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  newSize <- newTVar 0
  newVector <- V.replicateM initialSize (newTVar []) >>= newTVar
  return $ ConcurrentHashTable newVector newSize

-- | Returns size of the given hashtable
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable _ size') = atomically $ readTVar size'

-- | Returns Just value in the given CHT with the given key.
-- And Nothing if there is no such key
getCHT :: (Hashable k, Eq k) => Eq v => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable tvector _) = atomically $ do
    vector <- readTVar tvector
    let position = mod (hash key) $ V.length vector
    let cell = vector ! position
    curList <- readTVar cell
    return $ lookup key curList

-- | Insert given pair (key, value) to the given vector without any resizing and stuff.
-- Doesn't check whether the key is already there
insertElem :: (Hashable k) => V.Vector (TVar [(k, v)]) -> k -> v -> STM ()
insertElem vector newKey newValue = do
  let position = mod (hash newKey) $ V.length vector
  let cell = vector ! position
  curList <- readTVar cell
  let newList = (newKey, newValue) : curList
  writeTVar cell newList

-- | Update given key with the given value in the in the given association list
updateKey :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
updateKey newKey newValue = map (\(k, v) -> if k == newKey then (newKey, newValue) else (k, v))

-- | Insert given pair (key, value) to the given CHT, resize CHT if needed
putCHT :: (Hashable k, Eq k) => Eq v => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT newKey newValue (ConcurrentHashTable tvector tsize) = atomically $ do
    size <- readTVar tsize
    vector <- readTVar tvector
    let position = mod (hash newKey) $ V.length vector
    let cell = vector ! position
    curList <- readTVar cell
    case lookup newKey curList of
      Just _ -> writeTVar cell $ updateKey newKey newValue curList
      Nothing -> do writeTVar tsize (size + 1)
                    if fromIntegral size / fromIntegral (V.length vector) >= fillingRate
                    then do
                      let newCap = 2 * V.length vector
                      newVector <- V.replicateM newCap (newTVar [])
                      contents <- forM (V.toList vector) readTVar
                      let contentsList = (newKey, newValue) : concat contents
                      forM_ contentsList $ uncurry $ insertElem newVector
                      writeTVar tvector newVector
                    else writeTVar cell $ (newKey, newValue) : curList
