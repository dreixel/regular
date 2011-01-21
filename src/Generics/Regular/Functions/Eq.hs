{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Eq
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic equality.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Eq (
  
  -- * Generic equality
  Eq(..), eq
  
) where

import Generics.Regular.Base
import Prelude hiding (Eq)
import qualified Prelude as P (Eq)


class Eq f where
  eqf :: (a -> a -> Bool) -> f a -> f a -> Bool

instance Eq I where
  eqf f (I x) (I y) = f x y

instance P.Eq a => Eq (K a) where
  eqf _ (K x) (K y) = x == y

instance Eq U where
  eqf _ U U = True

instance (Eq f, Eq g) => Eq (f :+: g) where
  eqf f (L x) (L y) = eqf f x y
  eqf f (R x) (R y) = eqf f x y
  eqf _ _     _     = False

instance (Eq f, Eq g) => Eq (f :*: g) where
  eqf f (x1 :*: y1) (x2 :*: y2) = eqf f x1 x2 && eqf f y1 y2

instance Eq f => Eq (C c f) where
  eqf f (C x) (C y) = eqf f x y

instance Eq f => Eq (S s f) where
  eqf f (S x) (S y) = eqf f x y

eq :: (Regular a, Eq (PF a)) => a -> a -> Bool
eq x y = eqf eq (from x) (from y)
