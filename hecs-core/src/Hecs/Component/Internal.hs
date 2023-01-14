{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RoleAnnotations #-}
module Hecs.Component.Internal (
  ComponentId(..)
, Component(..)
, ViaStorable(..)
, StorableBackend(..)
, ArrayBackend(..)
, IterateBackend(..)
) where

import Hecs.Entity.Internal
import Hecs.HashTable.HashKey

import Foreign.Storable
import GHC.Exts
import Data.Proxy
import Data.Kind
import Data.Int
import Data.Word
import GHC.IO hiding (liftIO)
import Control.Monad.IO.Class

newtype ComponentId = ComponentId EntityId
  deriving stock Show
  deriving newtype (Eq, HashKey, Storable)

-- TODO Can we reasonably make a default instance?
class Coercible a (Store a) => Component a where
  type Backend a :: Type
  type Store a :: Type
  backing :: Proxy a -> (Backend a ~ ArrayBackend (Store a) => r) -> ((Backend a ~ StorableBackend (Store a), Storable (Store a)) => r) -> r

newtype ViaStorable a = ViaStorable a

instance Storable a => Component (ViaStorable a) where
  type Backend (ViaStorable a) = StorableBackend a
  type Store (ViaStorable a) = a
  backing _ _ flat = flat
  {-# INLINE backing #-}

deriving via (ViaStorable Int  ) instance Component Int
deriving via (ViaStorable Int8 ) instance Component Int8
deriving via (ViaStorable Int16) instance Component Int16
deriving via (ViaStorable Int32) instance Component Int32
deriving via (ViaStorable Int64) instance Component Int64

deriving via (ViaStorable Word  ) instance Component Word
deriving via (ViaStorable Word8 ) instance Component Word8
deriving via (ViaStorable Word16) instance Component Word16
deriving via (ViaStorable Word32) instance Component Word32
deriving via (ViaStorable Word64) instance Component Word64

deriving via (ViaStorable Float ) instance Component Float
deriving via (ViaStorable Double) instance Component Double

-- Iterating the backends
data ArrayBackend a = ArrayBackend Int# (MutableArray# RealWorld a)

data StorableBackend a = StorableBackend Int# (MutableByteArray# RealWorld)

class IterateBackend b a where
  iterateBackend :: MonadIO m => b a -> (a -> m (Maybe a)) -> m ()

instance IterateBackend ArrayBackend a where
  iterateBackend (ArrayBackend sz arr) f = go 0#
    where
      go n
        | isTrue# (n >=# sz) = pure ()
        | otherwise = do
          el <- liftIO $ IO (readArray# arr n)
          f el >>= \case
            Just newEl -> liftIO $ IO $ \s -> (# writeArray# arr n newEl s, () #)
            Nothing -> pure ()
          go (n +# 1#)
  {-# INLINE iterateBackend #-}

instance Storable a => IterateBackend StorableBackend a where
  iterateBackend (StorableBackend sz arr) f = go initAddr
    where
      initAddr = mutableByteArrayContents# arr
      lastAddr = initAddr `plusAddr#` (sz *# bSz)
      !(I# bSz) = sizeOf (undefined @_ @a)
      go addr
        | isTrue# (addr `eqAddr#` lastAddr) = pure ()
        | otherwise = do
          el <- liftIO $ peek (Ptr addr)
          f el >>= \case
            Just newEl -> liftIO $ poke (Ptr addr) newEl
            Nothing -> pure ()
          go (addr `plusAddr#` bSz)
  {-# INLINE iterateBackend #-}

