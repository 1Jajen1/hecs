{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Data.Int

import Control.Monad.IO.Class

import Hecs as Hecs

import GHC.Generics
import Foreign.Storable
import Control.Monad
import Data.Proxy

data Position = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Float {-# UNPACK #-} !Int
  deriving stock (Show, Generic)
  deriving (Storable, Component) via (GenericFlat Position)

makeWorld "World" [''Int, ''Int8, ''Position]

main :: IO ()
main = do
  w <- newWorld @World
  void . runHecsM w $ do
    eid <- newEntity
    setComponent @Int eid 10
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    e2 <- newEntity
    setComponent @Int e2 100
    setComponent @Position e2 (Pos 10 20 0)
    void . replicateM 1000 $ do
      car <- newEntity
      -- liftIO $ print car
      setComponent @Int car 100
      -- getComponent @Int car (liftIO . print) (pure ())
      setComponent car (Pos 10 0 50)
      -- getComponent @Position car (liftIO . print) (pure ())
      -- liftIO $ print $ sizeOf (undefined @_ @Position)
      -- liftIO $ print $ alignment (undefined @_ @Position)
      pure ()
    Hecs.filter (component @World @Int .&&. component @World @Position)
      (\aty _ -> do
        x <- getColumn @World @Int aty
        iterateBackend x $ \el -> pure $ Just (el + 1))
      (pure ())
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    getComponent @Int e2 (pure . Just) (pure Nothing) >>= liftIO . print
  putStrLn "Done"
