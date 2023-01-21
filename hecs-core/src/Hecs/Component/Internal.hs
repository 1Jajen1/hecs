{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.Component.Internal (
  ComponentId(..)
, Component(..)
, ComponentType(..)
, Column(..)
, ViaBox(..)
, ViaFlat(..)
, AccessColumn(..)
, IsComponent(..)
) where

import Hecs.Entity.Internal
import Foreign.Storable
import Data.Proxy
import GHC.Exts
import Hecs.HashTable.HashKey
import Data.Kind
import Data.Int
import Data.Word
import GHC.IO (IO(IO))
import Control.Monad.Base
import Data.Typeable

newtype ComponentId (c :: k) = ComponentId EntityId
  deriving newtype (Eq, Show, HashKey)

-- This is added to every instance of Component that is registered with TH
data IsComponent where
  IsComponent :: forall c . (Typeable c, Component c) => IsComponent
  deriving Component via ViaBox IsComponent

data ComponentType = Boxed | Flat

class Coercible (Value c) c => Component c where
  type ComponentKind c :: ComponentType
  type Value c :: Type
  backing :: Proxy c -> (ComponentKind c ~ Boxed => r) -> ((ComponentKind c ~ Flat, Storable (Value c)) => r) -> r

data family Column (ty :: ComponentType) c
data instance Column Boxed c = ColumnBoxed (MutableArray# RealWorld c)
data instance Column Flat  c = ColumnFlat  (MutableByteArray# RealWorld)

class AccessColumn (ty :: ComponentType) c where
  readColumn :: MonadBase IO m => Column ty c -> Int -> m c
  writeColumn :: MonadBase IO m => Column ty c -> Int -> c -> m ()

instance AccessColumn Boxed c where
  readColumn (ColumnBoxed arr) (I# n) = liftBase $ IO (readArray# arr n)
  {-# INLINE readColumn #-}
  writeColumn (ColumnBoxed arr) (I# n) el = liftBase $ IO $ \s -> case writeArray# arr n el s of s1 -> (# s1, () #)
  {-# INLINE writeColumn #-}

instance Storable c => AccessColumn Flat c where
  readColumn (ColumnFlat arr) n = liftBase $ peekElemOff (Ptr (mutableByteArrayContents# arr)) n
  {-# INLINE readColumn #-}
  writeColumn (ColumnFlat arr) n el = liftBase $ pokeElemOff (Ptr (mutableByteArrayContents# arr)) n el
  {-# INLINE writeColumn #-}

newtype ViaBox a = ViaBox a

instance Component (ViaBox a) where
  type ComponentKind (ViaBox a) = Boxed
  type Value (ViaBox a) = a
  backing _ b _ = b
  {-# INLINE backing #-}

newtype ViaFlat a = ViaFlat a
  deriving newtype Storable

instance Storable a => Component (ViaFlat a) where
  type ComponentKind (ViaFlat a) = Flat
  type Value (ViaFlat a) = a
  backing _ _ f = f
  {-# INLINE backing #-}

deriving via (ViaFlat Int  ) instance Component Int
deriving via (ViaFlat Int8 ) instance Component Int8
deriving via (ViaFlat Int16) instance Component Int16
deriving via (ViaFlat Int32) instance Component Int32
deriving via (ViaFlat Int64) instance Component Int64

deriving via (ViaFlat Word  ) instance Component Word
deriving via (ViaFlat Word8 ) instance Component Word8
deriving via (ViaFlat Word16) instance Component Word16
deriving via (ViaFlat Word32) instance Component Word32
deriving via (ViaFlat Word64) instance Component Word64

deriving via (ViaFlat Float ) instance Component Float
deriving via (ViaFlat Double) instance Component Double
