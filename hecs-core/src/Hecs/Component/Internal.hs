{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MagicHash #-}
module Hecs.Component.Internal (
  ComponentId(..)
, Component(..)
, ComponentType(..)
, Column(..)
, ViaBox(..)
, ViaFlat(..)
) where

import Hecs.Entity.Internal
import Foreign.Storable
import Data.Proxy
import GHC.Exts
import Hecs.HashTable.HashKey
import Data.Kind
import Data.Int
import Data.Word

newtype ComponentId (c :: k) = ComponentId EntityId
  deriving newtype (Eq, HashKey)

data ComponentType = Boxed | Flat

class Coercible (Value c) c => Component c where
  type ComponentKind c :: ComponentType
  type Value c :: Type
  backing :: Proxy c -> (ComponentKind c ~ Boxed => r) -> ((ComponentKind c ~ Flat, Storable (Value c)) => r) -> r

data family Column (ty :: ComponentType) c
data instance Column Boxed c = ColumnBoxed (MutableArray# RealWorld c)
data instance Column Flat  c = ColumnFlat  (MutableByteArray# RealWorld)

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
