{-# LANGUAGE TypeFamilies #-}
module Hecs.Component (
  ComponentId(..)
, Component(Backend)
, GenericFlat(..)
, ViaStorable(..)
, StorableBackend
, ArrayBackend
, IterateBackend(..)
, ViaBoxed(..)
, tag
) where

import Hecs.Component.Internal
import Hecs.Component.Generic
import Data.Kind

tag :: forall (a :: Type) . a
tag = undefined

newtype ViaBoxed a = ViaBoxed a

instance Component (ViaBoxed a) where
  type Backend (ViaBoxed a) = ArrayBackend a
  type (Store (ViaBoxed a)) = a
  backing _ b _ = b
  {-# INLINE backing #-}
