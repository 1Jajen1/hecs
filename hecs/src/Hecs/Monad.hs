{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.Monad (
  HecsM(..)
, runHecsM
) where

import Hecs.Monad.Class
import qualified Hecs.World as Core

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import Hecs.Filter
import Data.IORef

newtype HecsM w m a = HecsM { unHecsM :: ReaderT (IORef w) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b)  

runHecsM :: MonadIO m => w -> HecsM w m a -> m a
runHecsM w (HecsM f) = liftIO (newIORef w) >>= runReaderT f 
{-# INLINE runHecsM #-}

-- TODO Relax MonadIO to PrimMonad

instance (MonadBaseControl IO m, Core.WorldClass w) => MonadHecs w (HecsM w m) where
  newEntity = HecsM $ do
    wRef <- ask
    w <- liftBase $ readIORef wRef
    (w', eid) <- liftBase $ Core.allocateEntity w
    liftBase $ writeIORef wRef w'
    pure eid
  {-# INLINE newEntity #-}
  freeEntity eid = HecsM $ do
    wRef <- ask
    w <- liftBase $ readIORef wRef 
    w' <- liftBase $ Core.deAllocateEntity w eid
    liftBase $ writeIORef wRef w'
  {-# INLINE freeEntity #-}
  setComponentWithId eid compId comp = HecsM $ do
    wRef <- ask
    w <- liftBase $ readIORef wRef
    w' <- liftBase $ Core.setComponentWithId w eid compId comp
    liftBase $ writeIORef wRef w'
  {-# INLINE setComponentWithId #-}
  getComponentWithId eid compId s f = HecsM $ do
    w <- ask >>= liftBase . readIORef
    mC <- liftBase $ Core.getComponentWithId w eid compId (pure . Just) (pure Nothing)
    case mC of
      Nothing -> unHecsM f
      Just c -> unHecsM $ s c
  {-# INLINE getComponentWithId #-}
  filter :: forall b ty . Filter ty HasMainId -> (TypedArchetype ty -> b -> HecsM w m b) -> HecsM w m b -> HecsM w m b
  filter fi f z = HecsM ask >>= \wRef -> do
    st <- liftBaseWith $ \runInBase -> do
      w <- readIORef wRef
      Core.filter @_ @_ @(StM m b) w fi
        (\aty acc -> runInBase $ restoreM acc >>= f aty)
        (runInBase z)
    restoreM st
  {-# INLINE filter #-}
