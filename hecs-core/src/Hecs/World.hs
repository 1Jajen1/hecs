{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.World (
  newWorld
, Has
, allocateEntity
, deAllocateEntity
, getComponent, getComponentWithId
, hasTag, hasTagWithId
, addTag, addTagWithId
, addComponent, addComponentWithId
, setComponent, setComponentWithId
, removeTag, removeTagWithId
, removeComponent, removeComponentWithId
, WorldClass
, WorldImpl
, forFilter
, defer
, sync
) where

import Prelude hiding (filter)

import Hecs.Entity.Internal (EntityId)
import Hecs.Component.Internal

import Hecs.World.Internal
import Hecs.Filter

import Data.Proxy
import Control.Monad.Base
import Control.Monad.Trans.Control

newWorld :: (WorldClass w, MonadBase IO m) => m w
newWorld = liftBase new

getComponent :: forall c w r m . (WorldClass w, Component c, Has w c, MonadBaseControl IO m) => w -> EntityId -> (c -> m r) -> m r -> m r
getComponent w eid = getComponentWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getComponent #-}

getComponentWithId :: forall c w r m . (WorldClass w, Component c, MonadBaseControl IO m) => w -> EntityId -> ComponentId c -> (c -> m r) -> m r -> m r
getComponentWithId w eid cid s f = do
  st <- liftBaseWith $ \runInBase -> getComponentI w eid cid (runInBase . s) (runInBase f)
  restoreM st 
{-# INLINE getComponentWithId #-}

hasTag :: forall c w m . (WorldClass w, Has w c, MonadBase IO m) => w -> EntityId -> m Bool
hasTag w eid = hasTagWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE hasTag #-}

hasTagWithId :: forall c w m . (WorldClass w, MonadBase IO m) => w -> EntityId -> ComponentId c -> m Bool
hasTagWithId w eid compId = liftBase $ hasTagI w eid compId
{-# INLINE hasTagWithId #-}

addTag :: forall c w m . (WorldClass w, Has w c, MonadBase IO m) => w -> EntityId -> m ()
addTag w eid = addTagWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addTag #-}

addTagWithId :: forall c w m . (WorldClass w, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
addTagWithId w eid compId = liftBase $ addTagI w eid compId
{-# INLINE addTagWithId #-}

addComponent :: forall c w m . (WorldClass w, Component c, Has w c, MonadBase IO m) => w -> EntityId -> m ()
addComponent w eid = addComponentWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE addComponent #-}

addComponentWithId :: forall c w m . (WorldClass w, Component c, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
addComponentWithId w eid cid = liftBase $ addComponentI w eid cid
{-# INLINE addComponentWithId #-}

setComponent :: forall c w m . (WorldClass w, Component c, Has w c, MonadBase IO m) => w -> EntityId -> c -> m ()
setComponent w eid = setComponentWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE setComponent #-}

setComponentWithId :: forall c w m . (WorldClass w, Component c, MonadBase IO m) => w -> EntityId -> ComponentId c -> c -> m ()
setComponentWithId w eid cid c = liftBase $ setComponentI w eid cid c
{-# INLINE setComponentWithId #-}

removeTag :: forall c w m . (WorldClass w, Has w c, MonadBase IO m) => w -> EntityId -> m ()
removeTag w eid = removeTagWithId w eid (getComponentId @_ @_ @c (Proxy @w)) 
{-# INLINE removeTag #-}

removeTagWithId :: forall c w m . (WorldClass w, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
removeTagWithId w eid cid = liftBase $ removeTagI w eid cid
{-# INLINE removeTagWithId #-}

removeComponent :: forall c w m . (WorldClass w, Component c, Has w c, MonadBase IO m) => w -> EntityId -> m ()
removeComponent w eid = removeComponentWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE removeComponent #-}

removeComponentWithId :: forall c w m . (WorldClass w, Component c, MonadBase IO m) => w -> EntityId -> ComponentId c -> m ()
removeComponentWithId w eid cid = liftBase $ removeComponentI w eid cid
{-# INLINE removeComponentWithId #-}

forFilter :: (WorldClass w, MonadBaseControl IO m) => w -> Filter ty HasMainId -> (TypedArchetype ty -> b -> m b) -> m b -> m b
forFilter w fi f z = do
  st <- liftBaseWith $ \runInBase -> filterI w fi (\aty b -> runInBase $ restoreM b >>= f aty) (runInBase z)
  restoreM st
{-# INLINE forFilter #-}
