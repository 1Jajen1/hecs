{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Hecs.Monad (
  HecsM(..)
, runHecsM
) where

import Hecs.Monad.Class
import qualified Hecs.World as Core

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import Hecs.Filter

newtype HecsM w m a = HecsM { unHecsM :: StateT w m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b)  

runHecsM :: Monad m => w -> HecsM w m a -> m a
runHecsM w (HecsM stM) = evalStateT stM w
{-# INLINE runHecsM #-}

-- TODO Relax MonadIO to PrimMonad

instance (MonadBaseControl IO m, Core.WorldClass w) => MonadHecs w (HecsM w m) where
  newEntity = HecsM $ do
    w <- get
    (w', eid) <- liftBase $ Core.allocateEntity w
    put w'
    pure eid
  {-# INLINE newEntity #-}
  freeEntity eid = HecsM $ get >>= liftBase . flip Core.deAllocateEntity eid >>= put
  {-# INLINE freeEntity #-}
  setComponentWithId eid compId comp = HecsM $ do
    w <- get
    w' <- liftBase $ Core.setComponentWithId w eid compId comp
    put w'
  {-# INLINE setComponentWithId #-}
  getComponentWithId eid compId s f = HecsM $ do
    w <- get
    mC <- liftBase $ Core.getComponentWithId w eid compId (pure . Just) (pure Nothing)
    case mC of
      Nothing -> unHecsM f
      Just c -> unHecsM $ s c
  {-# INLINE getComponentWithId #-}
  filter :: forall b ty . Filter ty HasMainId -> (TypedArchetype ty -> b -> HecsM w m b) -> HecsM w m b -> HecsM w m b
  filter fi f z = HecsM get >>= \w -> do
    st <- liftBaseWith $ \runInBase -> do
      Core.filter @_ @_ @(StM m (b, w)) w fi
        (\aty acc -> runInBase $ restoreM acc >>= f aty)
        (runInBase z)
    restoreM st
  {-# INLINE filter #-}
