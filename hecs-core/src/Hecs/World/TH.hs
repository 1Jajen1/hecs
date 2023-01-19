{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
module Hecs.World.TH (
  makeWorld
) where

import Language.Haskell.TH

import Hecs.World.Internal
import Hecs.Component.Internal
import Hecs.Entity.Internal

import Hecs.Component

import Data.Proxy
import Hecs.Filter
import Control.Monad.Base

makeWorld :: String -> [Name] -> Q [Dec]
makeWorld wN names = do
  let wName = mkName wN
      worldImplName = mkName "WorldImpl"
      wldDec = NewtypeD [] wName [] Nothing (NormalC wName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT worldImplName) (LitT $ NumTyLit preAllocComps))]) []
      wCon = pure $ ConT wName
      natTy = pure . LitT $ NumTyLit preAllocComps
      mkHasInstance :: Int -> Name -> Q [Dec]
      mkHasInstance eid name = [d|
          instance {-# OVERLAPS #-} Has $wCon $cCon where
            getComponentId _ = ComponentId $ EntityId eid
            {-# INLINE getComponentId #-}
          {-# SPECIALISE syncSetComponent :: WorldImpl $natTy -> EntityId -> ComponentId $cCon -> $cCon -> IO () #-}
        |]
        where cCon = pure $ ConT name
  compInstances <- foldr (\(i, n) acc -> acc >>= \xs -> mkHasInstance i n >>= \ys -> pure $ ys ++ xs) (pure []) $ zip [1..] names
  otherInstances <- [d|
      deriving newtype instance WorldClass $wCon

      instance (Has $wCon l, Has $wCon r) => Has $wCon (Rel l r) where
        getComponentId _ = mkRelation (getComponentId (Proxy @($wCon))) (getComponentId (Proxy @($wCon)))
        {-# INLINE getComponentId #-}
    |]
  specializedApi <- [d|
      getComponentId :: Has $wCon c => ComponentId c
      getComponentId = Hecs.World.Internal.getComponentId (Proxy @($wCon))
      {-# INLINE getComponentId #-}

      component :: forall c . Has $wCon c => Filter c HasMainId
      component = Hecs.Filter.component (Proxy @($wCon))
      {-# INLINE component #-}

      filterDSL :: forall xs . FilterDSL $wCon (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
      filterDSL = Hecs.Filter.filterDSL @($wCon) @xs
      {-# INLINE filterDSL #-}

      getColumn :: forall c ty m . (Has $wCon c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Backend c)
      getColumn ty = Hecs.Filter.getColumn @c @($wCon) @ty @m ty
      {-# INLINE getColumn #-}
    |]
  pure $ wldDec : compInstances ++ otherInstances ++ specializedApi
  where
    preAllocComps = fromIntegral $ length names
