{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Component.Relation (
  Rel
) where

import Hecs.Component.Internal
import Data.Coerce
import Data.Proxy
import Data.Kind

newtype Rel l r = Rel ComponentId

type family Backing (l :: Type) (a :: k) (b :: k) :: k  where
  Backing (ArrayBackend _) a _ = a
  Backing (StorableBackend _) a _ = a
  Backing TagBackend _ b = b

instance (Component l, Component r, Coercible (Rel l r) (Store (Rel l r))) => Component (Rel l r) where
  type Backend (Rel l r) = Backing (Backend l) (Backend l) (Backend r)
  type (Store (Rel l r)) = Backing (Backend l) (Store l) (Store r)
  backing _ b f t = backing (Proxy @l)
    b
    f
    (backing (Proxy @r) b f t)
  {-# INLINE backing #-}
