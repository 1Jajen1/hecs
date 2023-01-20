{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Hecs.Monad.Class (
  MonadHecs(..)
, setComponent
, getComponent
, addTag
, hasTag
, addComponent
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
  addTagWithId :: forall {k} c . Core.EntityId -> Core.ComponentId (c :: k) -> m ()
  addComponentWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> m ()
  setComponentWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> c -> m ()
  getComponentWithId :: Core.Component c => Core.EntityId -> Core.ComponentId c -> (c -> m r) -> m r -> m r
  hasTagWithId :: forall {k} (c :: k) . Core.EntityId -> Core.ComponentId c -> m Bool
  filter :: Core.Filter ty Core.HasMainId -> (Core.TypedArchetype ty -> b -> m b) -> m b -> m b
  defer :: m a -> m a
  sync :: m ()

addComponent :: forall c w m . (MonadHecs w m, Core.Has w c, Core.Component c) => Core.EntityId -> m ()
addComponent eid = addComponentWithId eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addComponent #-}

setComponent :: forall c w m . (MonadHecs w m, Core.Has w c, Core.Component c) => Core.EntityId -> c -> m ()
setComponent eid comp = setComponentWithId eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w)) (coerce comp)
{-# INLINE setComponent #-}

getComponent :: forall c w m r . (MonadHecs w m, Core.Has w c, Core.Component c) => Core.EntityId -> (c -> m r) -> m r -> m r
getComponent eid = getComponentWithId eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getComponent #-}

addTag :: forall {k} (c :: k) w m . (MonadHecs w m, Core.Has w c) => Core.EntityId -> m ()
addTag eid = addTagWithId @_ @_ @c eid (Hecs.World.Internal.getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addTag #-}

hasTag :: forall c w m . (MonadHecs w m, Core.Has w c) => Core.EntityId -> m Bool
hasTag eid = hasTagWithId @w @m @c eid (Hecs.World.Internal.getComponentId (Proxy @w))
{-# INLINE hasTag #-}
