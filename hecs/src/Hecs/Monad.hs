{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.Monad (
  HecsM(..)
, runHecsM
, getWorld
) where

import Hecs.Monad.Class
import qualified Hecs.World as Core

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import Hecs.Filter
import Control.Monad.Trans.Class

newtype HecsM w m a = HecsM { unHecsM :: ReaderT w m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadBase b, MonadBaseControl b)  

runHecsM ::  w -> HecsM w m a -> m a
runHecsM w (HecsM f) = runReaderT f w
{-# INLINE runHecsM #-}

getWorld :: Monad m => HecsM w m w
getWorld = HecsM ask
{-# INLINE getWorld #-}

-- TODO Relax MonadIO to PrimMonad?

instance (MonadBaseControl IO m, Core.WorldClass w) => MonadHecs w (HecsM w m) where
  newEntity = HecsM $ ask >>= liftBase . Core.allocateEntity
  {-# INLINE newEntity #-}
  freeEntity eid = HecsM $ ask >>= liftBase . flip Core.deAllocateEntity eid
  {-# INLINE freeEntity #-}
  setComponentWithId eid compId comp = HecsM $ ask >>= \w -> liftBase $ Core.setComponentWithId w eid compId comp
  {-# INLINE setComponentWithId #-}
  getComponentWithId eid compId s (HecsM f) = HecsM $ ask >>= \w -> do
    st <- liftBaseWith $ \runInBase -> Core.getComponentWithId w eid compId
      (runInBase . unHecsM . s)
      (runInBase f)
    restoreM st
  {-# INLINE getComponentWithId #-}
  hasTagWithId eid compId = HecsM $ ask >>= \w -> liftBase $ Core.getComponentWithId w eid compId (const $ pure True) (pure False)
  filter :: forall b ty . Filter ty HasMainId -> (TypedArchetype ty -> b -> HecsM w m b) -> HecsM w m b -> HecsM w m b
  filter fi f z = HecsM ask >>= \w -> do
    st <- liftBaseWith $ \runInBase -> do
      Core.forFilter @_ @_ @(StM m b) w fi
        (\aty acc -> runInBase $ restoreM acc >>= f aty)
        (runInBase z)
    restoreM st
  {-# INLINE filter #-}
  defer act = do
    a <- HecsM . ReaderT $ \w -> restoreM =<< liftBaseWith (\runInBase -> Core.defer w $ \w' -> runInBase $ runHecsM w' act)
    sync
    pure a
  {-# INLINE defer #-}
  sync = HecsM $ ask >>= \w -> liftBase $ Core.sync w
  {-# INLINE sync #-}
