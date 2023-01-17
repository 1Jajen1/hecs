{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Component (
  ComponentId(..)
, Component(Backend)
, GenericFlat(..)
, ViaStorable(..)
, StorableBackend
, ArrayBackend
, TagBackend
, ComponentBackend(..)
, ViaBoxed(..)
, NoTagBackend
, ReadTagMsg
, ViaTag(..)
, IsTag
, Rel(..)
, mkRelation
, Tag
) where

import Hecs.Component.Internal
import Hecs.Component.Generic
import Hecs.Component.Relation

newtype ViaBoxed a = ViaBoxed a

instance Component (ViaBoxed a) where
  type Backend (ViaBoxed a) = ArrayBackend a
  type (Store (ViaBoxed a)) = a
  backing _ b _ _ = b
  {-# INLINE backing #-}

-- Tag is useful for when you want to get/set a dynamic Tag with ComponentId a but don't know what a should be. Simply use Tag in that case
data Tag
  deriving Component via (ViaTag Tag)

newtype ViaTag a = ViaTag a

instance Component (ViaTag a) where
  type Backend (ViaTag a) = TagBackend
  type Store (ViaTag a) = a
  backing _ _ _ t = t
  {-# INLINE backing #-}
