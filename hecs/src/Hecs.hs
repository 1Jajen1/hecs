{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs (
  makeWorld
, WorldClass
, WorldImpl
, newWorld
, runHecsM
, HecsM
, module Hecs.Component
, module Hecs.Monad.Class
, module Hecs.Filter
, component, componentWithId
, getColumn, getColumnWithId
, getEntityColumn
, EntityId(..)
, ComponentId(..)
, iterateArchetype
, readStored, writeStored
) where

import Data.Proxy

import Hecs.Monad
import Hecs.Monad.Class

import Hecs.Entity.Internal
import Hecs.Component hiding (readComponent, writeComponent)
import qualified Hecs.Component
import Hecs.World.TH
import Hecs.World
import qualified Hecs.World.Internal
import Hecs.Filter hiding (component, componentWithId, getColumnWithId, getEntityColumn, iterateArchetype)
import qualified Hecs.Filter
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

component :: forall w c . Has w c => Filter c HasMainId
component = Hecs.Filter.component (Proxy @w) (Proxy @c)
{-# INLINE component #-}

componentWithId :: forall c . Component c => ComponentId c -> Filter c HasMainId
componentWithId = Hecs.Filter.componentWithId (Proxy @c)
{-# INLINE componentWithId #-}

getColumn :: forall w c ty m . (Has w c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Backend c)
getColumn aty = getColumnWithId @c @ty aty (getComponentId @w @c)
{-# INLINE getColumn #-}

getColumnWithId :: forall c ty m . (Component c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> ComponentId c -> m (Backend c)
getColumnWithId aty c = liftBase $ Hecs.Filter.getColumnWithId (Proxy @c) aty c
{-# INLINE getColumnWithId #-}

getEntityColumn :: MonadBase IO m => TypedArchetype ty -> m (StorableBackend EntityId)
getEntityColumn aty = liftBase $ Hecs.Filter.getEntityColumn aty
{-# INLINE getEntityColumn #-}

getComponentId :: forall w c . Has w c => ComponentId c
getComponentId = Hecs.World.Internal.getComponentId (Proxy @w) (Proxy @c)
{-# INLINE getComponentId #-}

readStored :: (ComponentBackend b a, MonadIO m) => b a -> Int -> m a
readStored = Hecs.Component.readComponent
{-# INLINE readStored #-}

writeStored :: (ComponentBackend b a, MonadIO m) => b a -> Int -> a -> m ()
writeStored = Hecs.Component.writeComponent
{-# INLINE writeStored #-}

iterateArchetype :: forall ty m . MonadBaseControl IO m => TypedArchetype ty -> (Int -> EntityId -> m ()) -> m ()
iterateArchetype tyAty f = do
  st <- liftBaseWith $ \runInBase -> Hecs.Filter.iterateArchetype tyAty
    (\n eid st -> runInBase $ restoreM st >>= \() -> f n eid)
    (runInBase $ pure ())
  restoreM st
{-# INLINE iterateArchetype #-}
