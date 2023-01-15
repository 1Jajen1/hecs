{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
module Hecs.World.TH (
  makeWorld
) where

import Language.Haskell.TH

import Hecs.World.Internal
import Hecs.Component.Internal
import Hecs.Entity.Internal

import Data.Coerce

makeWorld :: String -> [Name] -> Q [Dec]
makeWorld wN names = do
  let wName = mkName wN
      worldImplName = mkName "WorldImpl"
      wldDec = NewtypeD [] wName [] Nothing (NormalC wName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT worldImplName) (LitT $ NumTyLit preAllocComps))]) []
      wCon = pure $ ConT wName
      -- natTy = pure . LitT $ NumTyLit preAllocComps
      mkHasInstance :: Int -> Name -> Q [Dec]
      mkHasInstance eid name = [d|
          instance {-# OVERLAPS #-} Has $wCon $cCon where
            getComponentId _ _ = ComponentId $ EntityId eid
            {-# INLINE getComponentId #-}
          -- {-# SPECIALISE setComponentI :: $wCon -> EntityId -> ComponentId -> $cCon -> IO $wCon #-}
        |]
        where cCon = pure $ ConT name
  compInstances <- foldr (\(i, n) acc -> acc >>= \xs -> mkHasInstance i n >>= \ys -> pure $ ys ++ xs) (pure []) $ zip [1..] names
  otherInstances <- [d|
      deriving newtype instance WorldClass $wCon
    |]
  pure $ wldDec : compInstances ++ otherInstances
  where
    preAllocComps = fromIntegral $ length names
