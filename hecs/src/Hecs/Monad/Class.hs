{-# LANGUAGE FunctionalDependencies #-}
module Hecs.Monad.Class (
  MonadHecs(..)
, setComponent
, getComponent
) where

import qualified Hecs.Entity.Internal as Core
import qualified Hecs.Component as Core
import qualified Hecs.World as Core
import qualified Hecs.World.Internal (getComponentId)
import qualified Hecs.Filter as Core

import Data.Proxy

-- TODO Get rid of internal imports, use higher level ones only to avoid relying on hecs-core internals here
-- TODO Consistency between names (hecs <-> hecs-core)

class MonadHecs w m | m -> w where
  withEntityAllocator :: m a -> m a
  newEntity :: m Core.EntityId
  freeEntity :: Core.EntityId -> m ()
  setComponentWithId :: Core.Component c => Core.EntityId -> Core.ComponentId -> c -> m ()
  getComponentWithId :: Core.Component c => Core.EntityId -> Core.ComponentId -> (c -> m r) -> m r -> m r
  filter :: Core.Filter ty Core.HasMainId -> (Core.TypedArchetype ty -> b -> m b) -> m b -> m b

setComponent :: forall c w m . (MonadHecs w m, Core.Has w c) => Core.EntityId -> c -> m ()
setComponent eid = setComponentWithId eid (Hecs.World.Internal.getComponentId (Proxy @w) (Proxy @c))
{-# INLINE setComponent #-}

getComponent :: forall c w m r . (MonadHecs w m, Core.Has w c) => Core.EntityId -> (c -> m r) -> m r -> m r
getComponent eid = getComponentWithId eid (Hecs.World.Internal.getComponentId (Proxy @w) (Proxy @c))
{-# INLINE getComponent #-}
