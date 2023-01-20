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

newtype Boxed = Boxed1 Int
  deriving Component via (ViaBox Boxed)

data Color = Red | Green | Blue
  deriving stock (Eq, Show)
  deriving Component via (ViaBox Color)

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
      setComponent @Boxed eid $ Boxed1 10
      setComponent @Int eid 10
      getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    e2 <- newEntity
    addTag @Test e2
    setComponent e2 (Rel @Color @(Wrap Red) Red)
    liftIO $ putStrLn "Set tag!"
    getComponent @(Rel Color (Wrap Red)) e2 (pure . Just) (pure Nothing) >>= liftIO . print
    setComponent @Int e2 100
    setComponent e2 (Pos 10 20 0)
    addTag @Red e2
    hasTag @Red e2 >>= liftIO . print
    void . replicateM 1000 $ do
      car <- newEntity
      liftIO $ print car
      setComponent @Int car 100
      getComponent @Int car (liftIO . print) (pure ())
      setComponent car (Pos 10 0 50)
      getComponent @Position car (liftIO . print) (pure ())
      liftIO $ print $ sizeOf (undefined @_ @Position)
      liftIO $ print $ alignment (undefined @_ @Position)
      pure ()
    Hecs.filter (filterDSL @'[Int, Wrap Red, Or Boxed Position])
      (\aty _ -> do
        x <- getColumn @Int aty
        es <- getEntityColumn aty
        iterateArchetype aty $ \n e -> do
          liftIO $ print e
          -- readColumn x n >>= liftIO . print 
        pure ()
          )
      (pure ())
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    getComponent @Int e2 (pure . Just) (pure Nothing) >>= liftIO . print
    hasTag @Red e2 >>= liftIO . print
  putStrLn "Done"
