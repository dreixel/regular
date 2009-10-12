{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.GMap
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Monadic generic map.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.GMap (

  -- * Functorial map function
  Functor (..),
  
  -- * Monadic functorial map function
  GMap (..)

) where

import Control.Monad

import Generics.Regular.Base


-----------------------------------------------------------------------------
-- Monadic functorial map function.
-----------------------------------------------------------------------------

-- | The @GMap@ class defines a monadic functorial map.
class GMap f where
  fmapM :: Monad m => (a -> m b) -> f a -> m (f b)

instance GMap I where
  fmapM f (I r) = liftM I (f r)

instance GMap (K a) where
  fmapM _ (K x)  = return (K x)

instance GMap U where
  fmapM _ U = return U

instance (GMap f, GMap g) => GMap (f :+: g) where
  fmapM f (L x) = liftM L (fmapM f x)
  fmapM f (R x) = liftM R (fmapM f x)

instance (GMap f, GMap g) => GMap (f :*: g) where
  fmapM f (x :*: y) = liftM2 (:*:) (fmapM f x) (fmapM f y)

instance GMap f => GMap (C c f) where
  fmapM f (C x) = liftM C (fmapM f x)

instance GMap f => GMap (S s f) where
  fmapM f (S x) = liftM S (fmapM f x)
