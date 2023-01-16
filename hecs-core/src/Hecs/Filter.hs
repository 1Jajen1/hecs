{-# LANGUAGE UndecidableInstances #-}
module Hecs.Filter (
  Filter
, FilterContext(HasMainId)
, (.&&.), (.||.)
, not
, componentWithId
, component
, TypedArchetype(..)
, getColumnWithId
, TypedHas
, iterateArchetype
) where

import Prelude hiding (not)

import Hecs.Filter.Internal
import Hecs.World.Internal

import Data.Proxy

component :: forall c w . Has w c => Proxy w -> Proxy c -> Filter c HasMainId
component w c = componentWithId c $ getComponentId w c
