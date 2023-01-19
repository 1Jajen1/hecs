{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Hecs.Monad.Class (
  MonadHecs(..)
, setComponent
, getComponent
, setTag, setTagWithId
, hasTag
) where

import qualified Hecs.Entity.Internal as Core
import qualified Hecs.Component.Internal as Core
import qualified Hecs.World as Core
import qualified Hecs.World.Internal (getComponentId)
import qualified Hecs.Filter as Core

import Data.Proxy
import Data.Kind
import Data.Coerce

-- TODO Get rid of internal imports, use higher level ones only to avoid relying on hecs-core internals here
-- TODO Consistency between names (hecs <-> hecs-core)

class Monad m => MonadHecs (w :: Type) (m :: Type -> Type) | m -> w where
  newEntity :: m Core.EntityId
  freeEntity :: Core.EntityId -> m ()
  setComponentWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> Core.Store c -> m ()
  getComponentWithId :: (Core.NoTagBackend (Core.Backend c) Core.ReadTagMsg, Core.Component c, Coercible (Core.Store c) c) => Core.EntityId -> Core.ComponentId c -> (c -> m r) -> m r -> m r
  hasTagWithId :: (Core.IsTag (Core.Backend c) "hasTag only allowed for tag components", Core.Component c) => Core.EntityId -> Core.ComponentId c -> m Bool
  filter :: Core.Filter ty Core.HasMainId -> (Core.TypedArchetype ty -> b -> m b) -> m b -> m b
  defer :: m a -> m a
  sync :: m ()

setComponent :: forall c w m . (MonadHecs w m, Core.Has w c, Coercible (Core.Store c) c) => Core.EntityId -> c -> m ()
setComponent eid comp = setComponentWithId eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w)) (coerce comp)
{-# INLINE setComponent #-}

getComponent :: forall c w m r . (Core.NoTagBackend (Core.Backend c) Core.ReadTagMsg, MonadHecs w m, Core.Has w c, Coercible (Core.Store c) c) => Core.EntityId -> (c -> m r) -> m r -> m r
getComponent eid = getComponentWithId eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getComponent #-}

setTag :: forall c w m . (Core.IsTag (Core.Backend c) "setTag only allows tag components", MonadHecs w m, Core.Has w c) => Core.EntityId -> m ()
setTag eid = setComponentWithId @_ @_ @c eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w)) (error "Hecs.Monad.Class:setTag evaluated tag placeholder!")
{-# INLINE setTag #-}

setTagWithId :: forall c w m . (Core.IsTag (Core.Backend c) "setTagWithId only allows tag components", MonadHecs w m, Core.Component c) => Core.EntityId -> Core.ComponentId c -> m ()
setTagWithId eid tagId = setComponentWithId eid tagId (error "Hecs.Monad.Class:setTagWithId evaluated tag placeholder!")
{-# INLINE setTagWithId #-}

hasTag :: forall c w m . (Core.IsTag (Core.Backend c) "hasTag only allowed for tag components", MonadHecs w m, Core.Has w c) => Core.EntityId -> m Bool
hasTag eid = hasTagWithId @w @m @c eid (Hecs.World.Internal.getComponentId (Proxy @w))
{-# INLINE hasTag #-}
