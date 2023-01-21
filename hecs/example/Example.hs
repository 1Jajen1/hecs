{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Data.Int

import Control.Monad.IO.Class

import Hecs

import GHC.Generics
import Foreign.Storable
import Control.Monad

import Control.Concurrent

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
  w <- newWorld
  void . runHecsM w $ do
    eid <- newEntity
    defer $ do
      eid <- newEntity
      setComponent @Boxed eid $ Boxed1 10
      setComponent @Int eid 10
      getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    liftIO $ print 1
    getComponent @Int eid (pure . Just) (pure Nothing) >>= liftIO . print
    e2 <- newEntity
    addTag @Test e2
    setComponent e2 (Rel @Color @(Tag Red) Red)
    liftIO $ putStrLn "Set tag!"
    getComponent @(Rel Color (Tag Red)) e2 (pure . Just) (pure Nothing) >>= liftIO . print
    setComponent @Int e2 100
    setComponent e2 (Pos 10 20 0)
    addTag @Red e2
    hasTag @Red e2 >>= liftIO . print
    removeTag @Red e2
    void . replicateM 65537 $ do
      car <- newEntity
      liftIO $ print car
      setComponent @Int car 100
      getComponent @Int car (liftIO . print) (pure ())
      addTag @Red car
      setComponent @Position car (Pos 10 0 50)
      getComponent @Position car (liftIO . print) (pure ())
      freeEntity car
    liftIO $ putStrLn "Filter"
    Hecs.filter (filterDSL @'[Int, Tag Red, Not (Tag Blue), Or Boxed Position])
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
