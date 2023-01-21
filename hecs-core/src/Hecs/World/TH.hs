{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
module Hecs.World.TH (
  makeWorld
) where

import Language.Haskell.TH

import qualified Hecs.World
import Hecs.World.Internal
import Hecs.Component.Internal
import Hecs.Component.Relation
import Hecs.Entity.Internal

import Data.Proxy
import Hecs.Filter hiding (tag)
import Control.Monad.Base
import Data.Coerce
import Data.Bitfield

makeWorld :: String -> [Name] -> Q [Dec]
makeWorld wN names' = do
  let wName = mkName wN
      worldImplName = mkName "WorldImpl"
      names = [''IsComponent] <> names'
      preAllocComps = fromIntegral $ length names
      wldDec = NewtypeD [] wName [] Nothing (NormalC wName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT worldImplName) (LitT $ NumTyLit preAllocComps))]) []
      wCon = pure $ ConT wName
      natTy = pure . LitT $ NumTyLit preAllocComps
      mkHasInstance :: Int -> Name -> Q [Dec]
      mkHasInstance eid name = [d|
          instance Has $wCon $cCon where
            getComponentId _ = ComponentId . EntityId $ Bitfield eid -- TODO Maybe pack instead
            {-# INLINE getComponentId #-}
        |]
        where cCon = pure $ ConT name
      processName eid name = do
        reify name >>= \case
          DataConI{} -> [d|
              instance Has $wCon $(conT name) where
                getComponentId _ = ComponentId $ EntityId $ Bitfield eid
                {-# INLINE getComponentId #-}
              type instance CaseTag $(conT name) a _ = a
            |]
          TyConI{} -> do
            xs <- mkHasInstance eid name
            ys <- reifyInstances (mkName "Component") [ConT name] >>= \case
              [] -> [d|
                  type instance CaseTag $(conT name) a _ = a
                |]
              _ -> let cCon = conT name in [d|
                  type instance CaseTag $cCon _ b = b

                  {-# SPECIALISE syncSetComponent    :: WorldImpl $natTy -> EntityId -> ComponentId $cCon -> $cCon -> IO () #-}
                  {-# SPECIALISE syncAddComponent    :: WorldImpl $natTy -> EntityId -> ComponentId $cCon ->          IO () #-}
                  {-# SPECIALISE syncRemoveComponent :: WorldImpl $natTy -> EntityId -> ComponentId $cCon ->          IO () #-}
                |]
            pure $ xs ++ ys
          _ -> error "TODO"
  compInstances <- foldr (\(i, n) acc -> acc >>= \xs -> processName i n >>= \ys -> pure $ ys ++ xs) (pure []) $ zip [1..] names
  otherInstances <- [d|
      deriving newtype instance WorldClass $wCon

      instance (Has $wCon l, Has $wCon r) => Has $wCon (Rel l r) where
        getComponentId _ = mkRelation (getComponentId (Proxy @($wCon))) (getComponentId (Proxy @($wCon)))
        {-# INLINE getComponentId #-}
      instance Has $wCon x => Has $wCon (Tag (x :: k)) where
        getComponentId _ = coerce $ getComponentId @_ @_ @x (Proxy @($wCon))
        {-# INLINE getComponentId #-}
    |]
  newWorldD <- [d|
      newWorld :: MonadBase IO m => m $wCon
      newWorld = do
        w <- $(conE wName) <$> liftBase Hecs.World.Internal.new
        $(foldr (\(i :: Int, n) acc -> do
          reify n >>= \case
            DataConI{} -> [| pure () |]
            TyConI{} -> reifyInstances (mkName "Component") [ConT n] >>= \case
              [] -> acc
              _ -> [| $(acc) >> Hecs.World.setComponent w (EntityId (Bitfield i)) (IsComponent @($(conT n))) |]
            _ -> error "TODO"
          ) [| pure () |] $ zip [1..] names)
        pure w
    |]
  specializedApi <- [d|
      getComponentId :: Has $wCon c => ComponentId c
      getComponentId = Hecs.World.Internal.getComponentId (Proxy @($wCon))
      {-# INLINE getComponentId #-}

      component :: forall c . (Component c, Has $wCon c) => Filter c HasMainId
      component = Hecs.Filter.component (Proxy @($wCon))
      {-# INLINE component #-}

      filterDSL :: forall xs . FilterDSL $wCon (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
      filterDSL = Hecs.Filter.filterDSL @($wCon) @xs
      {-# INLINE filterDSL #-}

      getColumn :: forall c ty m . (Component c, Has $wCon c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Column (ComponentKind c) c)
      getColumn ty = Hecs.Filter.getColumn @c @($wCon) @ty @m ty
      {-# INLINE getColumn #-}
    |]
  pure $ wldDec : compInstances ++ otherInstances ++ specializedApi ++ newWorldD
