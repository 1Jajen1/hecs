{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Hecs.Filter.Internal (
  Filter(..)
, FilterContext(..)
, CombineCtx
, InvertCtx
, extractMainId
, evaluate
, (.&&.), (.||.)
, not
, componentWithId
, TypedArchetype(..)
, getColumnWithId
, TypedHas
) where

import Prelude hiding (not)
import qualified Prelude

import Hecs.Archetype.Internal
import Hecs.Component.Internal

import Data.Proxy
import Data.Kind
import GHC.TypeLits
import Data.Type.Bool hiding (Not)
import qualified Data.Type.Bool

data FilterContext = HasMainId | DoesNotHaveMainId

type family CombineCtx (l :: FilterContext) (r :: FilterContext) :: FilterContext where
  CombineCtx DoesNotHaveMainId r = r
  CombineCtx l _ = l

type family InvertCtx (ctx :: FilterContext) :: FilterContext where
  InvertCtx HasMainId = DoesNotHaveMainId
  InvertCtx DoesNotHaveMainId = HasMainId

extractMainId :: Filter tyF HasMainId -> ComponentId
extractMainId (WithMain i  _) = i
{-# INLINE extractMainId #-}

evaluate :: Filter tyF ctx -> Archetype -> Bool
evaluate (WithMain _ f) = f
evaluate (NotFilter _ f) = f
{-# INLINE evaluate #-}

-- TODO Check if this always fuses cleanly
data Filter tyF (ctx :: FilterContext) where
  WithMain :: !ComponentId -> (Archetype -> Bool) -> Filter tyF HasMainId
  NotFilter :: !ComponentId -> (Archetype -> Bool) -> Filter tyF DoesNotHaveMainId

data And l r

(.&&.) :: Filter tyL l -> Filter tyR r -> Filter (And tyL tyR) (CombineCtx l r)
WithMain lId f .&&. WithMain _ g = WithMain lId $ \a -> f a && g a
NotFilter _ f .&&. WithMain rId g = WithMain rId $ \a -> f a && g a
WithMain lId f .&&. NotFilter _ g = WithMain lId $ \a -> f a && g a
NotFilter lId f .&&. NotFilter _ g = NotFilter lId $ \a -> f a && g a
{-# INLINE (.&&.) #-}

data Or l r

(.||.) :: Filter tyL l -> Filter tyR r -> Filter (Or l r) (CombineCtx l r)
WithMain lId f .||. WithMain _ g = WithMain lId $ \a -> f a || g a
NotFilter _ f .||. WithMain rId g = WithMain rId $ \a -> f a || g a
WithMain lId f .||. NotFilter _ g = WithMain lId $ \a -> f a || g a
NotFilter lId f .||. NotFilter _ g = NotFilter lId $ \a -> f a || g a
{-# INLINE (.||.) #-}

data Not x

not :: Filter ty ctx -> Filter (Not ty) (InvertCtx ctx)
not (WithMain m f) = NotFilter m $ Prelude.not . f
not (NotFilter m f) = WithMain m $ Prelude.not . f
{-# INLINE not #-}

-- TODO 
-- This has a small inefficiency: The main component id is guaranteed to be there, so no point in rechecking
-- but since we don't know who or what the main id is, we have no choice here
componentWithId :: Component c => Proxy c -> ComponentId -> Filter c HasMainId
componentWithId p compId = WithMain compId $ \aty -> lookupComponent p aty compId (const True) False
{-# INLINE componentWithId #-}

newtype TypedArchetype ty = TypedArchetype Archetype

getColumnWithId :: (Component c, TypedHas ty c) => Proxy c -> TypedArchetype ty -> ComponentId -> IO (Backend c)
getColumnWithId p (TypedArchetype aty) compId = lookupComponent p aty compId
  (\col -> getColumn p aty col)
  (error "Hecs.Filter.Internal:getColumn Component that was on the type level wasn't on the value level")
{-# INLINE getColumnWithId #-}

type family TypedHas ty (c :: Type) :: Constraint where
  TypedHas ty c = If (TypedHasBool ty c) (() :: Constraint) (TypeError ('Text "No type level evidence that this archetype has a component typed: " :<>: ShowType c :$$: Text "You may want to use an unsafe access method"))

type family TypedHasBool ty (c :: Type) :: Bool where
  TypedHasBool (And l r) c = TypedHasBool l c || TypedHasBool r c
  TypedHasBool (Or l r) c = False -- This is a bit annoying, but if we have an Or, we cannot conclusively say we have a component
  TypedHasBool (Not l) c = Data.Type.Bool.Not (TypedHasBool l c)
  TypedHasBool c c = True
  TypedHasBool a b = False
