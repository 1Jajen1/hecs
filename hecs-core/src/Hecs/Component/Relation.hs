{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Component.Relation (
  Rel(..)
, mkRelation
) where

import Hecs.Component.Internal
import Data.Proxy
import Data.Kind
import Hecs.Entity.Internal
import Data.Bits

mkRelation :: ComponentId l -> ComponentId r -> ComponentId (Rel l r)
mkRelation (ComponentId (EntityId l)) (ComponentId (EntityId r)) = ComponentId (EntityId combined)
  where
    combined = l `unsafeShiftL` 32 .|. r

newtype Rel l r = Rel (Backing (Backend l) (Store l) (Store r)) -- This is not the correct type for ComponentId, but it makes GHC shut up without forcing me to use data

type family Backing (l :: Type) (a :: k) (b :: k) :: k  where
  Backing (ArrayBackend _) a _ = a
  Backing (StorableBackend _) a _ = a
  Backing TagBackend _ b = b

instance (Component l, Component r) => Component (Rel l r) where
  type Backend (Rel l r) = Backing (Backend l) (Backend l) (Backend r)
  type (Store (Rel l r)) = Backing (Backend l) (Store l) (Store r)
  backing _ b f t = backing (Proxy @l)
    b
    f
    (backing (Proxy @r) b f t)
  {-# INLINE backing #-}
