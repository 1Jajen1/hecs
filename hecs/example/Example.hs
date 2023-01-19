{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Int

import Control.Monad.IO.Class

import Hecs

import GHC.Generics
import Foreign.Storable
import Control.Monad

data Position = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Float {-# UNPACK #-} !Int
  deriving stock (Show, Generic)
  deriving (Storable, Component) via (GenericFlat Position)

data Test
  deriving stock Generic
  deriving Component via (ViaTag Test)

newtype Boxed = Boxed Int
  deriving Component via (ViaBoxed Boxed)

data Color = Red | Green | Blue
  deriving Component via (ViaTag Color)

makeWorld "World" [
    ''Int
  , ''Int8
  , ''Position
  , ''Test
  , ''Boxed
  , ''Color, 'Red, 'Green, 'Blue
  ]

main :: IO ()
main = do
  w <- newWorld @World
  void . runHecsM w $ do
    eid <- newEntity
    defer $ do
      eid <- newEntity
      setComponent @Boxed eid $ Boxed 10
      setComponent @Int eid 10
      -- getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    e2 <- newEntity
    setTag @Test e2
    setComponent e2 (Rel @Test @Int 10)
    liftIO $ putStrLn "Set tag!"
    setComponent @Int e2 100
    setComponent @Position e2 (Pos 10 20 0)
    setTag @Red e2
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
    Hecs.filter (filterDSL @'[Int, Or Boxed Position])
      (\aty _ -> do
        x <- getColumn @Int aty
        es <- getEntityColumn aty
        iterateArchetype aty $ \n e -> do
          liftIO $ print e
          readColumn x n >>= liftIO . print 
        pure ()
          )
      (pure ())
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    getComponent @Int e2 (pure . Just) (pure Nothing) >>= liftIO . print
    hasTag @Red e2 >>= liftIO . print
  putStrLn "Done"
