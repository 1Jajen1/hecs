{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.World.Internal (
  WorldImpl(..)
, WorldClass(..)
, Has(..)
, syncSetComponent, syncAddComponent, syncRemoveComponent
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
import Foreign.Storable (sizeOf, alignment)
import GHC.Exts (Any)
import Data.IORef
import Control.Concurrent.MVar
import Data.Bits
import Data.Kind
import Data.Bitfield

-- This is going to be wrapped by 'makeWorld "World" [''Comp1, ''Comp2, ...]' which enables
-- making some component ids static. The componentMap is then only used for unknown/dynamic components
data WorldImpl (preAllocatedEIds :: Nat) = WorldImpl {
  freshEIdRef       :: !(MVar EntityId.FreshEntityId) -- allocate unique entity ids with reuse
, entityIndexRef    :: !(IORef (IntMap ArchetypeRecord)) -- changes often and thus needs good allround performance
, componentIndexRef :: !(IORef (HTB.HashTable (ComponentId Any) (Arr.Array ArchetypeRecord))) -- changes infrequently if ever after the component graph stabilises, so only read perf matters, but also not too much
, archetypeIndexRef :: !(IORef (HTB.HashTable ArchetypeTy Archetype)) -- changes infrequently once graph stabilises
, emptyArchetype    :: !Archetype
, deferredOpsRef    :: !(MVar (Arr.Array Command))
, isDeferred        :: !Bool -- TODO Experiment with this on the type level
}

-- TODO This is temporary and not very efficient yet
data Command =
    CreateEntity !EntityId
  | forall c .                AddTag          !EntityId !(ComponentId c)
  | forall c . Component c => AddComponent    !EntityId !(ComponentId c)
  | forall c . Component c => SetComponent    !EntityId !(ComponentId c) c
  | forall c .                RemoveTag       !EntityId !(ComponentId c)
  | forall c . Component c => RemoveComponent !EntityId !(ComponentId c)
  | DestroyEntity !EntityId


-- deferring updates:
-- Defer any component change: Entity Ids have a lock to allocate (Although I probably could defer them as well if I wanted to)
-- 
{-
  Common scenario for me:
    - a network thread allocates a new entity id for a new client
      - the network thread sets a bunch of components for this client, moving between a few tables
      - [Out] Client
    
    - the main thread iterates clients
      - we move clients to the Joined table with setting a tag
      - This has an [In] Client and [Out] Joined access pattern
    
    - we iterate a (changed) joined table
      -- This has [In] Client [In] Joined access pattern, so we have a data dependency from this system to the previous one

-- Defer everything in network threads by default
-- The main thread then commits at the start of a tick
-- Now we can utilise access patterns for concurrency or defer everything?

-}


data ArchetypeRecord = ArchetypeRecord !Int !Archetype

instance KnownNat n => WorldClass (WorldImpl n) where
  new = do
    freshEId' <- EntityId.new -- TODO Better init sz

    let entityIndex = mempty -- TODO Better init sz
    componentIndex <- HTB.new 32 -- TODO Better init sz
    archetypeIndex' <- HTB.new 32 -- TODO Better init sz
    emptyArchetype <- Archetype.empty

    archetypeIndex <- HTB.insert archetypeIndex' (Archetype.getTy emptyArchetype) emptyArchetype

    entityIndexRef <- newIORef entityIndex
    componentIndexRef <- newIORef componentIndex
    archetypeIndexRef <- newIORef archetypeIndex

    -- Preallocate a number of ids. This is a setup cost, but enables statically known ids for a list of components
    -- If this is ever too slow, this can be made much more efficient
    freshEId <- foldr (\_ feid -> do
      (st, eid) <- feid >>= EntityId.allocateEntityId
      row <- Archetype.addEntity emptyArchetype eid
      modifyIORef' entityIndexRef $ IM.insert (coerce eid) (ArchetypeRecord row emptyArchetype)
      pure st
      ) (pure freshEId') [0..preAllocatedEIds]

    freshEIdRef <- newMVar freshEId

    deferred <- Arr.new 8
    deferredOpsRef <- newMVar deferred

    let isDeferred = False

    pure WorldImpl{..}
    where
      preAllocatedEIds = fromIntegral @_ @Int $ natVal (Proxy @n)
  allocateEntity w@WorldImpl{..} = do
    eid <- modifyMVar freshEIdRef EntityId.allocateEntityId -- TODO Strictness

    if isDeferred
      then modifyMVar_ deferredOpsRef (`Arr.writeBack` CreateEntity eid) -- TODO Strictness
      else syncAllocateEntity w eid

    pure eid
  {-# INLINE allocateEntity #-}
  deAllocateEntity w@WorldImpl{..} eid = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` DestroyEntity eid) -- TODO Strictness
    else syncDestroyEntity w eid
  {-# INLINE deAllocateEntity #-}
  addTagI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` AddTag eid compId) -- TODO Strictness
    else syncAddTag w eid compId
  {-# INLINE addTagI #-}
  addComponentI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` AddComponent eid compId) -- TODO Strictness
    else syncAddComponent w eid compId
  {-# INLINE addComponentI #-}
  setComponentI w@WorldImpl{..} eid compId comp = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` SetComponent eid compId comp) -- TODO Strictness
    else syncSetComponent w eid compId comp
  {-# INLINE setComponentI #-}
  getComponentI :: forall c r . Component c => WorldImpl n -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  getComponentI WorldImpl{entityIndexRef} eid compId s f = do
    readIORef entityIndexRef >>= (\case
      Just (ArchetypeRecord row aty) -> Archetype.lookupComponent aty compId (Archetype.readComponent (Proxy @c) aty row >=> s) f
      Nothing -> f) . IM.lookup (coerce eid)
  {-# INLINE getComponentI #-}
  hasTagI :: forall c . WorldImpl n -> EntityId -> ComponentId c -> IO Bool
  hasTagI WorldImpl{entityIndexRef} eid compId = do
    readIORef entityIndexRef >>= (\case
      Just (ArchetypeRecord _ aty) -> Archetype.hasTag aty compId (const $ pure True) (pure False)
      Nothing -> pure False) . IM.lookup (coerce eid)
  {-# INLINE hasTagI #-}
  removeTagI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` RemoveTag eid compId) -- TODO Strictness
    else syncRemoveTag w eid compId
  {-# INLINE removeTagI #-}
  removeComponentI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` RemoveComponent eid compId) -- TODO Strictness
    else syncRemoveComponent w eid compId
  {-# INLINE removeComponentI #-}
  -- TODO Check if ghc removes the filter entirely
  filterI WorldImpl{componentIndexRef} fi f z = readIORef componentIndexRef >>= \componentIndex -> HTB.lookup componentIndex (Filter.extractMainId fi)
    (\arr ->
      let sz = Arr.size arr
          go !n !b
            | n >= sz   = pure b
            | otherwise = do
              ArchetypeRecord _ aty <- Arr.read arr n
              if Filter.evaluate fi aty
                then f (coerce aty) b >>= go (n + 1)
                else go (n + 1) b
      in z >>= go 0)
    z
  {-# INLINE filterI #-}
  defer w act = act $ w { isDeferred = True }
  sync w@WorldImpl{deferredOpsRef} = modifyMVar_ deferredOpsRef $ \arr -> go arr 0 >> Arr.new (max 8 $ Arr.size arr `unsafeShiftR` 2) -- TODO Better shrinking?
    where
      go !arr !n
        | n >= Arr.size arr = pure ()
        | otherwise = do
          Arr.read arr n >>= \case
            CreateEntity e -> syncAllocateEntity w e
            AddTag e cId -> syncAddTag w e cId
            AddComponent e cId -> syncAddComponent w e cId
            SetComponent e cId c -> syncSetComponent w e cId c
            RemoveTag e cId -> syncRemoveTag w e cId
            RemoveComponent e cId -> syncRemoveComponent w e cId
            DestroyEntity e -> syncDestroyEntity w e
          go arr (n + 1)

syncAllocateEntity :: WorldImpl n -> EntityId -> IO ()
syncAllocateEntity WorldImpl{..} eid = do
  row <- Archetype.addEntity emptyArchetype eid
  modifyIORef' entityIndexRef $ IM.insert (coerce eid) (ArchetypeRecord row emptyArchetype)

syncAdd ::
     (Archetype -> ArchetypeTy -> Int -> IO Archetype)
  -> (ArchetypeTy -> IO (ArchetypeTy, Int))
  -> (forall a . Archetype -> (Int -> IO a) -> IO a -> IO a)
  -> WorldImpl n -> EntityId -> ComponentId c -> IO (Archetype, Int, Int)
syncAdd newArchetype addToType lookupCol WorldImpl{..} eid compId = do
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row aty = IM.findWithDefault (error "Hecs.World.Internal:syncAdd entity id not in entity index!") (coerce eid) eIndex
  lookupCol aty (\c -> pure (aty, row, c)) $ Archetype.getEdge aty compId >>= \case
    ArchetypeEdge (Just dstAty) _ -> lookupCol dstAty (\c -> do
      (newRow, movedEid) <- Archetype.moveEntity aty row c dstAty
      -- Important insert the moved first in case it is ourselves so that we overwrite it after
      writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) eIndex
      pure (dstAty, newRow, c)
      )
      (error $ "Hecs.World.Internal:syncAdd edge destination did not have component: " <> show compId <> ". Searched in " <> show (getTy dstAty) )
    ArchetypeEdge Nothing _ -> do
      (newTy, newColumn) <- addToType (Archetype.getTy aty)

      archetypeIndex <- readIORef archetypeIndexRef
      dstAty <- HTB.lookup archetypeIndex newTy (\dstAty -> do
          -- putStrLn "Cheap move (no edge)" 
          Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
          pure dstAty
        ) $ do
          -- putStrLn "Expensive move" 
          dstAty <- newArchetype aty newTy newColumn

          Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
          !newArchetypeIndex <- HTB.insert archetypeIndex newTy dstAty
          writeIORef archetypeIndexRef newArchetypeIndex

          componentIndex <- readIORef componentIndexRef
          !compIndex <- Archetype.iterateComponentIds newTy (\tyId col ind -> do 
            arr <- HTB.lookup ind (coerce tyId) (`Arr.writeBack` ArchetypeRecord col dstAty) $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col dstAty)
            HTB.insert ind (coerce tyId) arr) (pure componentIndex)
          writeIORef componentIndexRef compIndex

          pure dstAty
      -- now move the entity and its current data between the two
      (newRow, movedEid) <- Archetype.moveEntity aty row newColumn dstAty

      -- Important insert the moved first in case it is ourselves so that we overwrite it after
      writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) eIndex

      pure (dstAty, newRow, newColumn)
{-# INLINE syncAdd #-}

syncAddTag :: WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncAddTag w eid compId = void $ syncAdd
  (\aty newTy _ -> Archetype.createArchetype newTy (getColumnSizes aty))
  (`Archetype.addTagType` compId)
  (`Archetype.hasTag` compId)
  w eid compId

syncAddComponent :: forall c n . Component c => WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncAddComponent w eid compId = void $ syncAdd
  (\aty newTy newColumn -> backing (Proxy @c)
      (Archetype.createArchetype newTy (getColumnSizes aty))
      (IO $ \s0 -> case Archetype.addColumnSize newColumn (sizeOf (undefined @_ @(Value c))) (alignment (undefined @_ @(Value c))) (Archetype.getColumnSizes aty) s0 of
        (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
          IO f -> f s1))
  (`Archetype.addComponentType` compId)
  (`Archetype.lookupComponent` compId)
  w eid compId
{-# INLINABLE syncAddComponent #-}

syncSetComponent :: forall c n . Component c => WorldImpl n -> EntityId -> ComponentId c -> c -> IO ()
syncSetComponent w eid compId comp = do
  (newAty, newRow, newCol) <- syncAdd
    (\aty newTy newColumn -> backing (Proxy @c)
      (Archetype.createArchetype newTy (getColumnSizes aty))
      (IO $ \s0 -> case Archetype.addColumnSize newColumn (sizeOf (undefined @_ @(Value c))) (alignment (undefined @_ @(Value c))) (Archetype.getColumnSizes aty) s0 of
        (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
          IO f -> f s1))
    (`Archetype.addComponentType` compId)
    (`Archetype.lookupComponent` compId)
    w eid compId
  Archetype.writeComponent (Proxy @c) newAty newRow newCol comp
{-# INLINABLE syncSetComponent #-}

syncRemove ::
     (Archetype -> ArchetypeTy -> Int -> IO Archetype)
  -> (ArchetypeTy -> Int -> IO ArchetypeTy)
  -> (forall a . Archetype -> (Int -> IO a) -> IO a -> IO a)
  -> WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncRemove newArchetype removeFromType lookupCol WorldImpl{..} eid compId = do
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row aty = IM.findWithDefault (error "Hecs.World.Internal:syncAdd entity id not in entity index!") (coerce eid) eIndex
  lookupCol aty (\removedColumn -> do
    dstAty <- Archetype.getEdge aty compId >>= \case
      ArchetypeEdge _ (Just dstAty) -> pure dstAty
      ArchetypeEdge _ Nothing -> do
        newTy <- removeFromType (Archetype.getTy aty) removedColumn

        archetypeIndex <- readIORef archetypeIndexRef
        HTB.lookup archetypeIndex newTy (\dstAty -> do
            -- putStrLn "Cheap move (no edge)" 
            Archetype.setEdge aty compId (ArchetypeEdge Nothing (Just dstAty))
            pure dstAty
          ) $ do
            -- putStrLn "Expensive move" 
            dstAty <- newArchetype aty newTy removedColumn

            Archetype.setEdge aty compId (ArchetypeEdge Nothing (Just dstAty))
            !newArchetypeIndex <- HTB.insert archetypeIndex newTy dstAty
            writeIORef archetypeIndexRef newArchetypeIndex

            componentIndex <- readIORef componentIndexRef
            !compIndex <- Archetype.iterateComponentIds newTy (\tyId col ind -> do 
              arr <- HTB.lookup ind (coerce tyId) (`Arr.writeBack` ArchetypeRecord col dstAty) $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col dstAty)
              HTB.insert ind (coerce tyId) arr) (pure componentIndex)
            writeIORef componentIndexRef compIndex

            pure dstAty
    
    -- now move the entity and its current data between the two
    (newRow, movedEid) <- Archetype.moveEntity aty row removedColumn dstAty

    -- Important insert the moved first in case it is ourselves so that we overwrite it after
    writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) eIndex
    ) $ pure ()
{-# INLINE syncRemove #-}


syncRemoveTag :: WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncRemoveTag w eid compId = void $ syncRemove
  (\aty newTy _ -> Archetype.createArchetype newTy (getColumnSizes aty))
  Archetype.removeTagType
  (`Archetype.hasTag` compId)
  w eid compId

syncRemoveComponent :: forall c n . Component c => WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncRemoveComponent w eid compId = void $ syncRemove
  (\aty newTy removedColumn -> backing (Proxy @c)
      (Archetype.createArchetype newTy (getColumnSizes aty))
      (IO $ \s0 -> case Archetype.removeColumnSize removedColumn (Archetype.getColumnSizes aty) s0 of
        (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
          IO f -> f s1))
  (Archetype.removeComponentType (Proxy @c))
  (`Archetype.lookupComponent` compId)
  w eid compId
{-# INLINABLE syncRemoveComponent #-}

syncDestroyEntity :: WorldImpl n -> EntityId -> IO ()
syncDestroyEntity WorldImpl{..} eid = do
  modifyMVar_ freshEIdRef $ flip EntityId.deAllocateEntityId eid -- TODO Strictness
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row aty = IM.findWithDefault (error "Hecs.World.Internal:syncAdd entity id not in entity index!") (coerce eid) eIndex
  movedEid <- Archetype.removeEntity aty row

  writeIORef entityIndexRef $! IM.delete (coerce eid) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) eIndex

-- A mapping from World -> ComponentId. A ComponentId from one World is not valid in another
class Has (w :: Type) (c :: k) where
  getComponentId :: Proxy w -> ComponentId c

-- All behavior a World has to support. makeWorld creates a newtype around WorldImpl and derives this
class WorldClass w where
  new :: IO w
  allocateEntity :: w -> IO EntityId
  deAllocateEntity :: w -> EntityId -> IO ()
  addTagI :: w -> EntityId -> ComponentId c -> IO ()
  addComponentI :: Component c => w -> EntityId -> ComponentId c -> IO ()
  setComponentI :: Component c => w -> EntityId -> ComponentId c -> c -> IO ()
  getComponentI :: Component c => w -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  hasTagI :: w -> EntityId -> ComponentId c -> IO Bool
  removeTagI :: w -> EntityId -> ComponentId c -> IO ()
  removeComponentI :: Component c => w -> EntityId -> ComponentId c -> IO ()
  filterI :: w -> Filter ty Filter.HasMainId -> (Filter.TypedArchetype ty -> b -> IO b) -> IO b -> IO b
  defer :: w -> (w -> IO a) -> IO a
  sync :: w -> IO ()

-- TODO I am probably (most likely) a little excessive on the inline/inlineable pragmas
-- I probably need them on functions that take continuation arguments. And inlineable on functions with typeclasses so that I can specialize them on import.
-- But other than that I should probably get rid of some 
