{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.LR
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functionality for regular dataypes: mapM, flatten, zip,
-- equality, show, value generation and fold.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.LR (

  -- * Functions for generating values that are different on top-level
  LRBase (..),
  LR (..),
  left,
  right,

) where

import Generics.Regular.Base


-----------------------------------------------------------------------------
-- Functions for generating values that are different on top-level.
-----------------------------------------------------------------------------

-- | The @LRBase@ class defines two functions, @leftb@ and @rightb@, which 
-- should produce different values.
class LRBase a where
  leftb  :: a
  rightb :: a

instance LRBase Int where
  leftb  = 0
  rightb = 1

instance LRBase Integer where
  leftb  = 0
  rightb = 1

instance LRBase Char where
  leftb  = 'L'
  rightb = 'R'
 
instance LRBase a => LRBase [a] where
  leftb  = []
  rightb = [rightb]

-- | The @LR@ class defines two functions, @leftf@ and @rightf@, which should 
-- produce different functorial values.
class LR f where
  leftf  :: a -> f a
  rightf :: a -> f a

instance LR I where
  leftf  x = I x
  rightf x = I x

instance LRBase a => LR (K a) where
  leftf  _ = K leftb
  rightf _ = K rightb

instance LR U where
  leftf  _ = U
  rightf _ = U

instance (LR f, LR g) => LR (f :+: g) where
  leftf  x = L (leftf x)
  rightf x = R (rightf x)

instance (LR f, LR g) => LR (f :*: g) where
  leftf  x = leftf x  :*: leftf x
  rightf x = rightf x :*: rightf x

instance LR f => LR (C c f) where
  leftf  x = C (leftf x)
  rightf x = C (rightf x)

instance LR f => LR (S s f) where
  leftf  x = S (leftf x)
  rightf x = S (rightf x)

-- | Produces a value which should be different from the value returned by 
-- @right@.
left :: (Regular a, LR (PF a)) => a
left = to (leftf left)

-- | Produces a value which should be different from the value returned by 
-- @left@.
right :: (Regular a, LR (PF a)) => a
right = to (rightf right)
