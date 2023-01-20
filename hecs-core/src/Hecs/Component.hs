{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hecs.Component (
  module Hecs.Component.Internal
, GenericFlat(..)
, Rel(..)
, mkRelation
) where

import Hecs.Component.Internal
import Hecs.Component.Generic
import Hecs.Component.Relation
