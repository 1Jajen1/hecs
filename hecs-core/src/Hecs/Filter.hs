{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hecs.Filter (
  Filter
, FilterContext(HasMainId)
, (.&&.), (.||.)
, not
, componentWithId
, component
, TypedArchetype(..)
, getColumn
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

import Hecs.Filter.Internal hiding (getColumnWithId, iterateArchetype, getEntityColumn)
import qualified Hecs.Filter.Internal
import Hecs.World.Internal
import Hecs.Component.Internal
import Hecs.Entity.Internal

import Data.Proxy
import GHC.TypeLits
import Data.Kind
import Control.Monad.Base
import Control.Monad.Trans.Control

component :: forall c w . Has w c => Proxy w -> Filter c HasMainId
component w = componentWithId $ getComponentId w
{-# INLINE component #-}

getColumn :: forall c w ty m . (Has w c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Backend c)
getColumn aty = getColumnWithId @c aty (getComponentId @_ @c (Proxy @w))
{-# INLINE getColumn #-}

getColumnWithId :: forall c ty m . (Component c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> ComponentId c -> m (Backend c)
getColumnWithId ty c = liftBase $ Hecs.Filter.Internal.getColumnWithId ty c
{-# INLINE getColumnWithId #-}

iterateArchetype :: MonadBaseControl IO m => TypedArchetype ty -> (Int -> EntityId -> m ()) -> m ()
iterateArchetype ty f = do
  st <- liftBaseWith $ \runInBase -> Hecs.Filter.Internal.iterateArchetype ty (\n eid acc -> runInBase $ restoreM acc >>= \() -> f n eid) (runInBase $ pure ())
  restoreM st
{-# INLINE iterateArchetype #-}

getEntityColumn :: MonadBase IO m => TypedArchetype ty -> m (StorableBackend EntityId)
getEntityColumn ty = liftBase $ Hecs.Filter.Internal.getEntityColumn ty
{-# INLINE getEntityColumn #-}

type family FilterFromList xs where
  FilterFromList (x:y:ys) = And x (FilterFromList (y:ys))
  FilterFromList '[x] = x
  FilterFromList '[] = TypeError ('Text "Cannot create an empty filter (yet)") -- TODO Empty filters could work?

type family HasMain ty :: FilterContext where
  HasMain (And l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Or l r) = CombineCtx (HasMain l) (HasMain r)
  HasMain (Not a) = InvertCtx (HasMain a)
  HasMain _ = HasMainId

filterDSL :: forall (w :: Type) xs . FilterDSL w (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
filterDSL = filterDSLI (Proxy @w) (Proxy @(FilterFromList xs))
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
  filterDSLI p _ = component p
  {-# INLINE filterDSLI #-}
