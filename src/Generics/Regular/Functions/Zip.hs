{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Zip
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic zip.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Zip (

  -- * Zip functions
  Zip (..),
  fzip,
  fzip'

) where

import Control.Monad (liftM, liftM2)

import Generics.Regular.Base


-----------------------------------------------------------------------------
-- Zip functions.
-----------------------------------------------------------------------------

-- | The @Zip@ class defines a monadic zip on functorial values.
class Zip f where
  fzipM :: Monad m => (a -> b -> m c) -> f a -> f b -> m (f c)

instance Zip I where
  fzipM f (I x) (I y) = liftM I (f x y)

instance Eq a => Zip (K a) where
  fzipM _ (K x) (K y) 
    | x == y    = return (K x)
    | otherwise = fail "fzipM: structure mismatch"

instance Zip U where
  fzipM _ U U = return U

instance (Zip f, Zip g) => Zip (f :+: g) where
  fzipM f (L x) (L y) = liftM L (fzipM f x y)
  fzipM f (R x) (R y) = liftM R (fzipM f x y)
  fzipM _ _       _       = fail "fzipM: structure mismatch"

instance (Zip f, Zip g) => Zip (f :*: g) where
  fzipM f (x1 :*: y1) (x2 :*: y2) = 
    liftM2 (:*:) (fzipM f x1 x2)
                 (fzipM f y1 y2)

instance Zip f => Zip (C c f) where
  fzipM f (C x) (C y) = liftM C (fzipM f x y)

instance Zip f => Zip (S s f) where
  fzipM f (S x) (S y) = liftM S (fzipM f x y)

-- | Functorial zip with a non-monadic function, resulting in a monadic value.
fzip  :: (Zip f, Monad m) => (a -> b -> c) -> f a -> f b -> m (f c)
fzip f = fzipM (\x y -> return (f x y))

-- | Partial functorial zip with a non-monadic function.
fzip' :: Zip f => (a -> b -> c) -> f a -> f b -> f c
fzip' f x y = maybe (error "fzip': structure mismatch") id (fzip f x y)
