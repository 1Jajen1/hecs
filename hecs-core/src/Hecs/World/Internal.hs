{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedTuples #-}
module Hecs.World.Internal (
  WorldImpl(..)
, WorldClass(..)
, Has(..)
) where

import qualified Hecs.Array as Arr
import Hecs.Archetype.Internal as Archetype
import Hecs.Component.Internal
import Hecs.Entity.Internal (EntityId(..))
import qualified Hecs.Entity.Internal as EntityId
import qualified Hecs.HashTable.Boxed as HTB
import Hecs.Filter.Internal (Filter)
import qualified Hecs.Filter.Internal as Filter

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Coerce
import GHC.IO
import Foreign.Storable (sizeOf)
import Debug.Trace

-- This is going to be wrapped by 'makeWorld "World" [''Comp1, ''Comp2, ...]' which enables
-- making some component ids static. The componentMap is then only used for unknown/dynamic components
data WorldImpl (preAllocatedEIds :: Nat) = WorldImpl {
  freshEId         :: !EntityId.FreshEntityId -- allocate unique entity ids with reuse
, entityIndex      :: !(IntMap ArchetypeRecord) -- changes often and thus needs good allround performance
, componentIndex   :: !(HTB.HashTable ComponentId (Arr.Array ArchetypeRecord)) -- changes infrequently if ever after the component graph stabilises, so only read perf matters, but also not too much
, archetypeIndex   :: !(HTB.HashTable ArchetypeTy Archetype) -- changes infrequently once graph stabilises
, emptyArchetype   :: !Archetype
}

data ArchetypeRecord = ArchetypeRecord !Int !Archetype

instance KnownNat n => WorldClass (WorldImpl n) where
  new = do
    freshEId' <- EntityId.new -- TODO Better init sz
    -- Preallocate a number of ids. This is a setup cost, but enables statically known ids for a list of components
    -- If this is ever too slow, this can be made much more efficient
    freshEId <- foldr (\_ feid -> fmap fst $ feid >>= EntityId.allocateEntityId) (pure freshEId') [0..preAllocatedEIds]

    let entityIndex = mempty -- TODO Better init sz
    componentIndex <- HTB.new 32 -- TODO Better init sz
    archetypeIndex' <- HTB.new 32 -- TODO Better init sz
    emptyArchetype <- Archetype.empty

    archetypeIndex <- HTB.insert archetypeIndex' (Archetype.getTy emptyArchetype) emptyArchetype

    pure WorldImpl{..}
    where
      preAllocatedEIds = fromIntegral @_ @Int $ natVal (Proxy @n)
  withEntityAllocator WorldImpl{freshEId} = EntityId.withEntityAllocator freshEId
  {-# INLINE withEntityAllocator #-}
  allocateEntity w@WorldImpl{entityIndex, freshEId, emptyArchetype} = do
    (freshEID', eid) <- EntityId.allocateEntityId freshEId
    row <- Archetype.addEntity emptyArchetype eid
    let eindex = IM.insert (coerce eid) (ArchetypeRecord row emptyArchetype) entityIndex
    pure (w { freshEId = freshEID', entityIndex = eindex }, eid)
  {-# INLINE allocateEntity #-}
  deAllocateEntity w@WorldImpl{freshEId} eid = do
    freshEID' <- EntityId.deAllocateEntityId freshEId eid
    -- TODO Actually remove the entity everywhere
    pure (w { freshEId = freshEID' })
  {-# INLINE deAllocateEntity #-}

  setComponentI :: forall c . Component c => WorldImpl n -> EntityId -> ComponentId -> c -> IO (WorldImpl n)
  setComponentI w@WorldImpl{entityIndex,archetypeIndex,componentIndex} eid compId comp = do
    let ArchetypeRecord row aty = IM.findWithDefault (error "Hecs.World.Internal:setComponentI entity id not in entity index!") (coerce eid) entityIndex
    -- Check if we have that component, if yes, write it, if no, move the entity
    Archetype.lookupComponent (Proxy @c) aty compId
      (\c -> do
        -- putStrLn "Write only"
        Archetype.writeComponent aty row c comp >> pure w)
      $ Archetype.getEdge aty compId >>= \case
        -- We have an edge! Move the entity and write the component there
        ArchetypeEdge (Just dstAty) _ -> Archetype.lookupComponent (Proxy @c) dstAty compId
          (\c -> do
            (newRow, movedEid) <- Archetype.moveEntity aty row c dstAty
            -- putStrLn "Cheap move (edge)" 

            Archetype.writeComponent dstAty newRow c comp

            -- Important insert the moved first in case it is ourselves so that we overwrite it after
            pure (w { entityIndex = IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) entityIndex }))
          (error "Hecs.World.Internal:setComponentI edge destination did not have component")
        -- We don't have an edge, but the archetype may exist, so check the archetype index first
        ArchetypeEdge Nothing _ -> do -- remove edge should be empty!
          (newTy, newColumn) <- Archetype.addComponentType (Proxy @c) (Archetype.getTy aty) compId
          (w', dstAty) <- HTB.lookup archetypeIndex newTy (\dstAty -> do
              -- putStrLn "Cheap move (no edge)" 
              Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
              pure (w, dstAty)
            ) $ do
              -- putStrLn "Expensive move" 
              dstAty <- backing (Proxy @c)
                (Archetype.createArchetype newTy (getColumnSizes aty))
                (IO $ \s0 -> case Archetype.addColumnSize newColumn (sizeOf (undefined @_ @(Store c))) (getColumnSizes aty) s0 of
                  (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
                    IO f -> f s1)
              Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
              newArchetypeIndex <- HTB.insert archetypeIndex newTy dstAty

              compIndex <- Archetype.iterateComponentIds newTy (\tyId ind -> do 
                arr <- HTB.lookup ind tyId (`Arr.writeBack` ArchetypeRecord newColumn dstAty) $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord newColumn dstAty)
                HTB.insert ind tyId arr) (pure componentIndex)

              pure (w { archetypeIndex = newArchetypeIndex, componentIndex = compIndex }, dstAty)

          -- now move the entity and its current data between the two
          (newRow, movedEid) <- Archetype.moveEntity aty row newColumn dstAty

          -- and finally write the component
          Archetype.writeComponent dstAty newRow newColumn comp

          -- Important insert the moved first in case it is ourselves so that we overwrite it after
          pure $ w' { entityIndex = IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) entityIndex }
  {-# INLINABLE setComponentI #-}
  getComponentI :: forall c r . Component c => WorldImpl n -> EntityId -> ComponentId -> (c -> IO r) -> IO r -> IO r
  getComponentI WorldImpl{entityIndex} eid compId s f = do
    let ArchetypeRecord row aty = IM.findWithDefault (error "Hecs.World.Internal:getComponentI entity id not in entity index!") (coerce eid) entityIndex
    Archetype.lookupComponent (Proxy @c) aty compId (Archetype.readComponent aty row >=> s) f
  {-# INLINE getComponentI #-}
  -- TODO Check if ghc removes the filter entirely
  filterI WorldImpl{componentIndex} fi f z = HTB.lookup componentIndex (Filter.extractMainId fi)
    (\arr ->
      let sz = Arr.size arr
          go !n !b
            | n >= sz   = pure b
            | otherwise = do
              ArchetypeRecord _ aty <- Arr.read arr n
              traceIO "Next record"
              if Filter.evaluate fi aty
                then traceIO "Match" >> f (coerce aty) b >>= go (n + 1)
                else traceIO "No match" >> go (n + 1) b
      in z >>= go 0)
    z
  {-# INLINE filterI #-}

-- A mapping from World -> ComponentId. A ComponentId from one World is not valid in another
class Component c => Has w c where
  getComponentId :: proxy w -> proxy c -> ComponentId

-- All behavior a World has to support. makeWorld creates a newtype around WorldImpl and derives this
class WorldClass w where
  new :: IO w
  withEntityAllocator :: w -> IO a -> IO a
  allocateEntity :: w -> IO (w, EntityId)
  deAllocateEntity :: w -> EntityId -> IO w
  setComponentI :: Component c => w -> EntityId -> ComponentId -> c -> IO w
  getComponentI :: Component c => w -> EntityId -> ComponentId -> (c -> IO r) -> IO r -> IO r
  filterI :: w -> Filter ty Filter.HasMainId -> (Filter.TypedArchetype ty -> b -> IO b) -> IO b -> IO b

-- TODO I am probably (most likely) a little excessive on the inline/inlineable pragmas
-- I probably need them on functions that take continuation arguments. And inlineable on functions with typeclasses so that I can specialize them on import.
-- But other than that I should probably get rid of some 
