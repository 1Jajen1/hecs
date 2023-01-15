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
, NoTagBackend
, ReadTagMsg
, ViaTag(..)
, IsTag
) where

import Hecs.Component.Internal
import Hecs.Component.Generic

newtype ViaBoxed a = ViaBoxed a

instance Component (ViaBoxed a) where
  type Backend (ViaBoxed a) = ArrayBackend a
  type (Store (ViaBoxed a)) = a
  backing _ b _ _ = b
  {-# INLINE backing #-}

newtype ViaTag a = ViaTag a

instance Component (ViaTag a) where
  type Backend (ViaTag a) = TagBackend
  type Store (ViaTag a) = a
  backing _ _ _ t = t
  {-# INLINE backing #-}
