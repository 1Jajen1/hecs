{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Hecs.Filter (
  Filter
, FilterContext(HasMainId)
, (.&&.), (.||.)
, not
, componentWithId
, component
, TypedArchetype(..)
, getColumnWithId
, getEntityColumn
, TypedHas
, iterateArchetype
, And, Or, Not
, FilterDSL
, filterDSL
, FilterFromList
, HasMain
) where

import Prelude hiding (not)

import Hecs.Filter.Internal
import Hecs.World.Internal

import Data.Proxy
import GHC.TypeLits
import Data.Kind

component :: forall c w . Has w c => Proxy w -> Proxy c -> Filter c HasMainId
component w c = componentWithId c $ getComponentId w c

type family FilterFromList xs where
  FilterFromList (x:y:ys) = And x (FilterFromList (y:ys))
  FilterFromList '[x] = x
  FilterFromList '[] = TypeError ('Text "Cannot create an empty filter (yet)") -- TODO Empty filters could work?

type family HasMain ty :: FilterContext where
  HasMain (And l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Or l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Not a) = InvertCtx (HasMain a)
  HasMain _ = HasMainId

filterDSL :: forall (w :: Type) xs . FilterDSL w (FilterFromList xs) => Proxy w -> Proxy xs -> Filter (FilterFromList xs) (HasMain (FilterFromList xs))
filterDSL _ _ = filterDSLI (Proxy @w) (Proxy @(FilterFromList xs))
{-# INLINE filterDSL #-}

class FilterDSL w ty where
  filterDSLI :: Proxy w -> Proxy ty -> Filter ty (HasMain ty)

instance {-# OVERLAPPING #-} FilterDSL w f => FilterDSL w (Not f) where
  filterDSLI p _ = not $ filterDSLI p (Proxy @f)
  {-# INLINE filterDSLI #-}

instance {-# OVERLAPPING #-} (FilterDSL w l, FilterDSL w r) => FilterDSL w (Or l r) where
  filterDSLI p _ = filterDSLI p (Proxy @l) .||. filterDSLI p (Proxy @r) 
  {-# INLINE filterDSLI #-}

instance {-# OVERLAPPING #-} (FilterDSL w l, FilterDSL w r) => FilterDSL w (And l r) where
  filterDSLI p _ = filterDSLI p (Proxy @l) .&&. filterDSLI p (Proxy @r) 
  {-# INLINE filterDSLI #-}

instance {-# INCOHERENT #-} (Has w c, HasMain c ~ HasMainId) => FilterDSL w c where
  filterDSLI p _ = component p (Proxy @c)
  {-# INLINE filterDSLI #-}
