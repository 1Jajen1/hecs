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
, ComponentBackend(..)
, TagBackend
, NoTagBackend
, ReadTagMsg
, IsTag
) where

import Hecs.Entity.Internal
import Hecs.HashTable.HashKey

import Foreign.Storable
import GHC.Exts
import Data.Proxy
import Data.Kind
import Data.Int
import Data.Word ( Word8, Word16, Word32, Word64 )
import GHC.IO hiding (liftIO)
import GHC.TypeLits
import Control.Monad.Base

newtype ComponentId c = ComponentId EntityId
  deriving stock Show
  deriving newtype (Eq, HashKey, Storable)

type family NoTagBackend a err :: Constraint where
  NoTagBackend TagBackend err = TypeError ('Text err)
  NoTagBackend _ _ = ()

type ReadTagMsg = "Cannot read a tag"

type family IsTag a err :: Constraint where
  IsTag TagBackend _ = ()
  IsTag a err = TypeError ('Text err :$$: ('Text "Cannot match " :<>: ShowType a :<>: 'Text " with expected TagBackend"))

-- TODO Can we reasonably make a default instance?
class Coercible a (Store a) => Component a where
  type Backend a :: Type
  type Store a :: Type
  backing :: Proxy a
    -> (Backend a ~ ArrayBackend (Store a) => r)
    -> ((Backend a ~ StorableBackend (Store a), Storable (Store a)) => r)
    -> (Backend a ~ TagBackend => r)
    -> r

newtype ViaStorable a = ViaStorable a

instance Storable a => Component (ViaStorable a) where
  type Backend (ViaStorable a) = StorableBackend a
  type Store (ViaStorable a) = a
  backing _ _ flat _ = flat
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

data ArrayBackend a = ArrayBackend (MutableArray# RealWorld a)

data StorableBackend a = StorableBackend (MutableByteArray# RealWorld)

data TagBackend

class ComponentBackend b a where
  readColumn :: MonadBase IO m => b a -> Int -> m a
  writeColumn :: MonadBase IO m => b a -> Int -> a -> m ()

instance ComponentBackend ArrayBackend a where
  readColumn (ArrayBackend arr) (I# n) = liftBase $ IO (readArray# arr n)
  {-# INLINE readColumn #-}
  writeColumn (ArrayBackend arr) (I# n) el = liftBase . IO $ \s -> case writeArray# arr n el s of s1 -> (# s1, () #)
  {-# INLINE writeColumn #-}

instance Storable a => ComponentBackend StorableBackend a where
  readColumn (StorableBackend arr) n = liftBase $ peekElemOff (Ptr (mutableByteArrayContents# arr)) n
  {-# INLINE readColumn #-}
  writeColumn (StorableBackend arr) n el = liftBase $ pokeElemOff (Ptr (mutableByteArrayContents# arr)) n el
  {-# INLINE writeColumn #-}
