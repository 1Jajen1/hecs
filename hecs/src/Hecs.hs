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
, EntityId(..)
, ComponentId(..)
) where

import Data.Proxy

import Hecs.Monad
import Hecs.Monad.Class

import Hecs.Entity.Internal
import Hecs.Component
import Hecs.World.TH
import Hecs.World
import Hecs.World.Internal (Has(..))
import Hecs.Filter hiding (component, componentWithId, getColumnWithId)
import qualified Hecs.Filter
import Control.Monad.Base

component :: forall w c . Has w c => Filter c HasMainId
component = Hecs.Filter.component (Proxy @w) (Proxy @c)

componentWithId :: forall c . Component c => ComponentId -> Filter c HasMainId
componentWithId = Hecs.Filter.componentWithId (Proxy @c)

getColumn :: forall w c ty m . (Has w c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Backend c)
getColumn aty = getColumnWithId @c @ty aty (getComponentId (Proxy @w) (Proxy @c))

getColumnWithId :: forall c ty m . (Component c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> ComponentId -> m (Backend c)
getColumnWithId aty c = liftBase $ Hecs.Filter.getColumnWithId (Proxy @c) aty c
