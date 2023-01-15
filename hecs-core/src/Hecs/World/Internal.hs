{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedTuples #-}
module Hecs.World.Internal (
  WorldImpl(..)
, WorldClass(..)
, Has(..)
, syncSetComponent
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
import GHC.Exts (Any)
import Data.IORef
import Control.Concurrent.MVar
import Data.Bits

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

-- TODO

{- Commands:

CreateEntity
AddComponent
SetComponent
RemoveComponent
FreeEntity

-}

-- TODO This is temporary and not very efficient yet
data Command =
    CreateEntity !EntityId
  | forall c . Component c => SetComponent !EntityId !(ComponentId c) c
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
  setComponentI :: forall c . Component c => WorldImpl n -> EntityId -> ComponentId c -> c -> IO ()
  setComponentI w@WorldImpl{..} eid compId comp = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` SetComponent eid compId comp) -- TODO Strictness
    else syncSetComponent w eid compId comp
  {-# INLINE setComponentI #-}
  getComponentI :: forall c r . Component c => WorldImpl n -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  getComponentI WorldImpl{entityIndexRef} eid compId s f = do
    readIORef entityIndexRef >>= (\case
      Just (ArchetypeRecord row aty) -> Archetype.lookupComponent (Proxy @c) aty compId (Archetype.readComponent aty row >=> s) f
      Nothing -> f) . IM.lookup (coerce eid)
  {-# INLINE getComponentI #-}
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
            SetComponent e cId c -> syncSetComponent w e cId c
            DestroyEntity e -> syncDestroyEntity w e
          go arr (n + 1)

syncAllocateEntity :: WorldImpl n -> EntityId -> IO ()
syncAllocateEntity WorldImpl{..} eid = do
  row <- Archetype.addEntity emptyArchetype eid
  modifyIORef' entityIndexRef $ IM.insert (coerce eid) (ArchetypeRecord row emptyArchetype)

syncSetComponent :: forall c n . Component c => WorldImpl n -> EntityId -> ComponentId c -> c -> IO ()
syncSetComponent WorldImpl{..} eid compId comp = do
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row aty = IM.findWithDefault (error "Hecs.World.Internal:setComponentI entity id not in entity index!") (coerce eid) eIndex
  -- Check if we have that component, if yes, write it, if no, move the entity
  Archetype.lookupComponent (Proxy @c) aty compId
    (\c -> do
      -- putStrLn "Write only"
      Archetype.writeComponent aty row c comp)
    $ Archetype.getEdge aty compId >>= \case
      -- We have an edge! Move the entity and write the component there
      ArchetypeEdge (Just dstAty) _ -> Archetype.lookupComponent (Proxy @c) dstAty compId
        (\c -> do
          (newRow, movedEid) <- Archetype.moveEntity aty row c dstAty
          -- putStrLn "Cheap move (edge)" 

          Archetype.writeComponent dstAty newRow c comp

          -- Important insert the moved first in case it is ourselves so that we overwrite it after
          writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) eIndex)
        (error "Hecs.World.Internal:setComponentI edge destination did not have component")
      -- We don't have an edge, but the archetype may exist, so check the archetype index first
      ArchetypeEdge Nothing _ -> do -- remove edge should be empty!
        (newTy, newColumn) <- Archetype.addComponentType (Proxy @c) (Archetype.getTy aty) compId

        archetypeIndex <- readIORef archetypeIndexRef
        dstAty <- HTB.lookup archetypeIndex newTy (\dstAty -> do
            -- putStrLn "Cheap move (no edge)" 
            Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
            pure dstAty
          ) $ do
            -- putStrLn "Expensive move" 
            dstAty <- backing (Proxy @c)
              (Archetype.createArchetype newTy (getColumnSizes aty))
              (IO $ \s0 -> case Archetype.addColumnSize newColumn (sizeOf (undefined @_ @(Store c))) (getColumnSizes aty) s0 of
                (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
                  IO f -> f s1)
              (Archetype.createArchetype newTy (getColumnSizes aty))

            Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
            !newArchetypeIndex <- HTB.insert archetypeIndex newTy dstAty
            writeIORef archetypeIndexRef newArchetypeIndex

            componentIndex <- readIORef componentIndexRef
            !compIndex <- Archetype.iterateComponentIds newTy (\tyId ind -> do 
              arr <- HTB.lookup ind (coerce tyId) (`Arr.writeBack` ArchetypeRecord newColumn dstAty) $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord newColumn dstAty)
              HTB.insert ind (coerce tyId) arr) (pure componentIndex)
            writeIORef componentIndexRef compIndex

            -- TODO I cannot use w { ... } here because of componentIndex
            pure dstAty

        -- now move the entity and its current data between the two
        (newRow, movedEid) <- Archetype.moveEntity aty row newColumn dstAty

        -- and finally write the component
        Archetype.writeComponent dstAty newRow newColumn comp

        -- Important insert the moved first in case it is ourselves so that we overwrite it after
        writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row aty) eIndex
{-# INLINABLE syncSetComponent #-}

syncDestroyEntity :: WorldImpl n -> EntityId -> IO ()
syncDestroyEntity WorldImpl{..} eid = do
  modifyMVar_ freshEIdRef $ flip EntityId.deAllocateEntityId eid -- TODO Strictness
  -- TODO Actually remove the entity everywhere

-- A mapping from World -> ComponentId. A ComponentId from one World is not valid in another
class Component c => Has w c where
  getComponentId :: proxy w -> proxy c -> ComponentId c

-- All behavior a World has to support. makeWorld creates a newtype around WorldImpl and derives this
class WorldClass w where
  new :: IO w
  allocateEntity :: w -> IO EntityId
  deAllocateEntity :: w -> EntityId -> IO ()
  setComponentI :: Component c => w -> EntityId -> ComponentId c -> c -> IO ()
  getComponentI :: Component c => w -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  filterI :: w -> Filter ty Filter.HasMainId -> (Filter.TypedArchetype ty -> b -> IO b) -> IO b -> IO b
  defer :: w -> (w -> IO a) -> IO a
  sync :: w -> IO ()

-- TODO I am probably (most likely) a little excessive on the inline/inlineable pragmas
-- I probably need them on functions that take continuation arguments. And inlineable on functions with typeclasses so that I can specialize them on import.
-- But other than that I should probably get rid of some 
