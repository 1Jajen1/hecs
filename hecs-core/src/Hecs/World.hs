module Hecs.World (
  newWorld
, Has
, allocateEntity
, deAllocateEntity
, getComponent
, getComponentWithId
, setComponent
, setComponentWithId
, WorldClass
, WorldImpl
, filter
) where

import Prelude hiding (filter)

import Hecs.Entity.Internal (EntityId)
import Hecs.Component.Internal

import Hecs.World.Internal
import Hecs.Filter

import Data.Proxy

newWorld :: WorldClass w => IO w
newWorld = new

getComponent :: forall c w r . (WorldClass w, Has w c) => w -> EntityId -> (c -> IO r) -> IO r -> IO r
getComponent w eid = getComponentWithId w eid (getComponentId (Proxy @w) (Proxy @c))
{-# INLINE getComponent #-}

getComponentWithId :: forall c w r . (WorldClass w, Component c) => w -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
getComponentWithId = getComponentI
{-# INLINE getComponentWithId #-}

setComponent :: forall c w . (WorldClass w, Has w c) => w -> EntityId -> c -> IO ()
setComponent w eid = setComponentWithId w eid (getComponentId (Proxy @w) (Proxy @c))
{-# INLINE setComponent #-}

setComponentWithId :: forall c w . (WorldClass w, Component c) => w -> EntityId -> ComponentId c -> c -> IO ()
setComponentWithId = setComponentI
{-# INLINE setComponentWithId #-} -- TODO This is bad

filter :: WorldClass w => w -> Filter ty HasMainId -> (TypedArchetype ty -> b -> IO b) -> IO b -> IO b
filter = filterI
{-# INLINE filter #-}
