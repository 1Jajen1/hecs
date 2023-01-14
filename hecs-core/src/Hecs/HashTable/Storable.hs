{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Hecs.HashTable.Storable (
  HashTable
, new
, insert
, lookup
) where

import Hecs.HashTable.HashKey

import Data.Word
import Data.Bits
import Control.Monad
import Prelude hiding (lookup)
import Foreign.Storable
import GHC.Exts
import GHC.IO

-- Mutable linear hashtable
-- Used internally to map Components to their ids and will probably have some other uses later on
-- Requires both keys and values to be storable. So no gc objects can be stored, but primitive values can be stored unboxed.
data HashTable key value = HashTable {
  size    :: {-# UNPACK #-} !Int
, capMask :: {-# UNPACK #-} !Int
, backing :: !(MutableByteArray# RealWorld)
}

-- State <> key <> hash <> value
-- State indicates if this entry is full/deleted/empty
data Entry key value = Entry !Word8 !key !Int !value

instance (Storable key, Storable value) => Storable (Entry key value) where
  sizeOf _ = pad + sz
    where
      pad = mod sz 8
      sz = 1 + sizeOf (undefined :: key) + sizeOf (undefined :: value) + sizeOf (undefined :: Int)
  alignment _ = 8
  -- TODO Do aligned reads instead
  peekByteOff ptr off = Entry
    <$> peekByteOff (coerce ptr) off
    <*> peekByteOff (coerce ptr) (off + 1)
    <*> peekByteOff (coerce ptr) (off + 1 + sizeOf (undefined :: key))
    <*> peekByteOff (coerce ptr) (off + 1 + sizeOf (undefined :: key) + sizeOf (undefined :: Int))
  -- TODO Do aligned writes instead
  pokeByteOff ptr off (Entry t k h v) = do
    pokeByteOff (coerce ptr) off t
    pokeByteOff (coerce ptr) (off + 1) k
    pokeByteOff (coerce ptr) (off + 1 + sizeOf (undefined :: key)) h
    pokeByteOff (coerce ptr) (off + 1 + sizeOf (undefined :: key) + sizeOf (undefined :: Int)) v

new :: forall key value . (Storable key, Storable value) => Int -> IO (HashTable key value)
new initSz = IO $ \s ->
  case newPinnedByteArray# reqSz s of
    (# s', arr #) -> case setByteArray# arr 0# reqSz 0# s' of s'' -> (# s'', HashTable 0 (iSzP2 - 1) arr #)
  where
    iSzP2 :: Int
    iSzP2 = if initSz == 1 then 1 else 1 `unsafeShiftL` (8 * sizeOf (undefined :: Int) - countLeadingZeros (initSz - 1))
    !(I# reqSz) = iSzP2 * sizeOf (undefined :: Entry key value)

insert :: forall key value . (HashKey key, Storable key, Storable value) => HashTable key value -> key -> value -> IO (HashTable key value)
insert old@HashTable{..} k v = go start >>= \case
    True -> if size + 1 >= threeQuartersCap
      then resize (HashTable (size + 1) capMask backing)
      else pure (HashTable (size + 1) capMask backing)
    False -> pure old
  where
    threeQuartersCap = 3 * ((capMask + 1) `unsafeShiftR` 2)
    h = hash (hashKey k)
    start = h .&. capMask
    go :: Int -> IO Bool
    go n | n > capMask = go 0
    go n = do
      Entry full k1 h1 _ <- readByteArray @(Entry key value) backing n
      case full of
        1 -> if h1 == h && k1 == k
          then writeByteArray backing n (Entry 1 k h v) >> pure False
          else go (n + 1)
        _ -> writeByteArray backing n (Entry 1 k h v) >> pure True

unsafeInsert :: forall key value . (Eq key, Storable key, Storable value) => HashTable key value -> key -> Int -> value -> IO ()
unsafeInsert HashTable{..} k h v = go start
  where
    start = h .&. capMask
    go :: Int -> IO ()
    go n | n > capMask = go 0
    go n = do
      Entry full k1 h1 _ <- readByteArray @(Entry key value) backing n
      case full of
        1 -> if h1 == h && k1 == k
          then pure ()
          else go (n + 1)
        _ -> writeByteArray backing n (Entry 1 k h v)

lookup :: forall key value r . (HashKey key, Storable key, Storable value) => HashTable key value -> key -> (value -> IO r) -> IO r -> IO r
lookup HashTable{..} k cont notFound = go start
  where
    h = hash (hashKey k)
    start = h .&. capMask
    go :: Int -> IO r
    go n | n > capMask = go 0
    go n = do
      Entry full k1 h1 v1 <- readByteArray backing n
      case full of
        1 -> if h1 == h && k1 == k then cont v1 else go (n + 1)
        2 -> go (n + 1)
        _ -> notFound

iterateWithHash :: (Storable key, Storable value) => HashTable key value -> (key -> Int -> value -> IO ()) -> IO ()
iterateWithHash HashTable{..} f = go 0
  where
    go n | n > capMask = pure ()
    go n = readByteArray backing n >>= (\(Entry t k h v) -> when (t == 1) $ f k h v) >> go (n + 1)

resize :: forall key value . (Eq key, Storable key, Storable value) => HashTable key value -> IO (HashTable key value)
resize old@HashTable{..} = do
  newHT <- IO $ \s ->
    case newPinnedByteArray# reqSz s of
      (# s', arr #) -> case setByteArray# arr 0# reqSz 0# s' of s'' -> (# s'', HashTable size (((capMask + 1) * 2) - 1) arr #)
  iterateWithHash old $ \k h v -> unsafeInsert newHT k h v
  pure newHT
  where
    !(I# reqSz) = (capMask + 1) * 2 * sizeOf (undefined :: Entry key value)

readByteArray :: Storable a => MutableByteArray# RealWorld -> Int -> IO a
readByteArray arr off = 
  let addr# = mutableByteArrayContents# arr
  in peekElemOff (Ptr addr#) off 
{-# INLINE readByteArray #-}

writeByteArray :: Storable a => MutableByteArray# RealWorld -> Int -> a -> IO ()
writeByteArray arr off a =
  let addr# = mutableByteArrayContents# arr
  in pokeElemOff (Ptr addr#) off a 
{-# INLINE writeByteArray #-}
