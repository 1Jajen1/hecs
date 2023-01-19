{-# LANGUAGE AllowAmbiguousTypes #-}
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

getComponent :: forall c w r m . (WorldClass w, Has w c, MonadBaseControl IO m) => w -> EntityId -> (Store c -> m r) -> m r -> m r
getComponent w eid = getComponentWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE getComponent #-}

getComponentWithId :: forall c w r m . (WorldClass w, Component c, MonadBaseControl IO m) => w -> EntityId -> ComponentId c -> (Store c -> m r) -> m r -> m r
getComponentWithId w eid cid s f = do
  st <- liftBaseWith $ \runInBase -> getComponentI w eid cid (runInBase . s) (runInBase f)
  restoreM st 
{-# INLINE getComponentWithId #-}

setComponent :: forall c w m . (WorldClass w, Has w c, MonadBase IO m) => w -> EntityId -> Store c -> m ()
setComponent w eid = setComponentWithId w eid (getComponentId @_ @_ @c (Proxy @w))
{-# INLINE setComponent #-}

setComponentWithId :: forall c w m . (WorldClass w, Component c, MonadBase IO m) => w -> EntityId -> ComponentId c -> Store c -> m ()
setComponentWithId w eid cid c = liftBase $ setComponentI w eid cid c
{-# INLINE setComponentWithId #-}

forFilter :: (WorldClass w, MonadBaseControl IO m) => w -> Filter ty HasMainId -> (TypedArchetype ty -> b -> m b) -> m b -> m b
forFilter w fi f z = do
  st <- liftBaseWith $ \runInBase -> filterI w fi (\aty b -> runInBase $ restoreM b >>= f aty) (runInBase z)
  restoreM st
{-# INLINE forFilter #-}
