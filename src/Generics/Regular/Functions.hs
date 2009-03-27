{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Rewriting.Base
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functionality for regular dataypes: map, flatten, zip,
-- equality, show and value generation.
-----------------------------------------------------------------------------

module Generics.Regular.Functions (

  -- * Functorial map function.
  Functor (..),
  
  -- * Monadic functorial map function.
  GMap (..),
  
  -- * Crush functions.
  Crush (..),
  flatten,

  -- * Zip functions.
  Zip (..),
  fzip,
  fzip',

  -- * Equality function.
  geq,

  -- * Show function.
  GShow (..),
  
  -- * Functions for generating values that are different on top-level.
  LRBase (..),
  LR (..),
  left,
  right  

) where

import Control.Monad

import Generics.Regular.Base


-----------------------------------------------------------------------------
-- Functorial map function.
-----------------------------------------------------------------------------

instance Functor Id where
  fmap f (Id r) = Id (f r)

instance Functor (K a) where
  fmap _ (K a) = K a

instance Functor Unit where
  fmap _ Unit = Unit

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L x) = L (fmap f x)
  fmap f (R y) = R (fmap f y)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = fmap f x :*: fmap f y

instance Functor f => Functor (Con f) where
  fmap f (Con con r) = Con con (fmap f r)


-----------------------------------------------------------------------------
-- Monadic functorial map function.
-----------------------------------------------------------------------------

-- | The @GMap@ class defines a monadic functorial map.
class GMap f where
  fmapM :: Monad m => (a -> m b) -> f a -> m (f b)

instance GMap Id where
  fmapM f (Id r) = liftM Id (f r)

instance GMap (K a) where
  fmapM _ (K x)  = return (K x)

instance GMap Unit where
  fmapM _ Unit = return Unit

instance (GMap f, GMap g) => GMap (f :+: g) where
  fmapM f (L x) = liftM L (fmapM f x)
  fmapM f (R x) = liftM R (fmapM f x)

instance (GMap f, GMap g) => GMap (f :*: g) where
  fmapM f (x :*: y) = liftM2 (:*:) (fmapM f x) (fmapM f y)

instance GMap f => GMap (Con f) where
  fmapM f (Con c x) = liftM (Con c) (fmapM f x)


-----------------------------------------------------------------------------
-- Crush functions.
-----------------------------------------------------------------------------

-- | The @Crush@ class defines a crush on functorial values. In fact,
-- @crush@ is a generalized @foldr@.
class Crush f where
  crush :: (a -> b -> b) -> b -> f a -> b

instance Crush Id where
  crush op e (Id x) = x `op` e

instance Crush (K a) where
  crush _ e _ = e

instance Crush Unit where
  crush _ e _ = e

instance (Crush f, Crush g) => Crush (f :+: g) where
  crush op e (L x) = crush op e x
  crush op e (R y) = crush op e y

instance (Crush f, Crush g) => Crush (f :*: g) where
  crush op e (x :*: y) = crush op (crush op e y) x

instance Crush f => Crush (Con f) where
  crush op e (Con _c x) = crush op e x

-- | Flatten a structure by collecting all the elements present.
flatten :: Crush f => f a -> [a]
flatten = crush (:) []


-----------------------------------------------------------------------------
-- Zip functions.
-----------------------------------------------------------------------------

-- | The @Zip@ class defines a monadic zip on functorial values.
class Zip f where
  fzipM :: Monad m => (a -> b -> m c) -> f a -> f b -> m (f c)

instance Zip Id where
  fzipM f (Id x) (Id y) = liftM Id (f x y)

instance Eq a => Zip (K a) where
  fzipM _ (K x) (K y) 
    | x == y    = return (K x)
    | otherwise = fail "fzipM: structure mismatch"

instance Zip Unit where
  fzipM _ Unit Unit = return Unit

instance (Zip f, Zip g) => Zip (f :+: g) where
  fzipM f (L x) (L y) = liftM L (fzipM f x y)
  fzipM f (R x) (R y) = liftM R (fzipM f x y)
  fzipM _ _       _       = fail "fzipM: structure mismatch"

instance (Zip f, Zip g) => Zip (f :*: g) where
  fzipM f (x1 :*: y1) (x2 :*: y2) = 
    liftM2 (:*:) (fzipM f x1 x2)
                 (fzipM f y1 y2)

instance Zip f => Zip (Con f) where
  fzipM f (Con c1 x) (Con _c2 y) = liftM (Con c1) (fzipM f x y)

-- | Functorial zip with a non-monadic function, resulting in a monadic value.
fzip  :: (Zip f, Monad m) => (a -> b -> c) -> f a -> f b -> m (f c)
fzip f = fzipM (\x y -> return (f x y))

-- | Partial functorial zip with a non-monadic function.
fzip' :: Zip f => (a -> b -> c) -> f a -> f b -> f c
fzip' f x y = maybe (error "fzip': structure mismatch") id (fzip f x y)


-----------------------------------------------------------------------------
-- Equality function.
-----------------------------------------------------------------------------

-- | Equality on values based on their structural representation.
geq :: (b ~ PF a, Regular a, Crush b, Zip b) => a -> a -> Bool
geq x y = maybe False (crush (&&) True) (fzip geq (from x) (from y))


-----------------------------------------------------------------------------
-- Show function.
-----------------------------------------------------------------------------

-- | The @GShow@ class defines a show on values.
class GShow f where
  gshow :: (a -> ShowS) -> f a -> ShowS

instance GShow Id where
  gshow f (Id r) = f r

instance Show a => GShow (K a) where
  gshow _ (K x) = shows x

instance GShow Unit where
  gshow _ Unit = id

instance (GShow f, GShow g) => GShow (f :+: g) where
  gshow f (L x) = gshow f x
  gshow f (R x) = gshow f x

instance (GShow f, GShow g) => GShow (f :*: g) where
  gshow f (x :*: y) = gshow f x . showChar ' ' . gshow f y

instance GShow f => GShow (Con f) where
  gshow f (Con c x) = showParen True (showString c . showChar ' ' . gshow f x)


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

instance LRBase Char where
  leftb  = 'L'
  rightb = 'R'
 
instance LRBase a => LRBase [a] where
  leftb  = []
  rightb = [error "Should never be inspected"]

-- | The @LR@ class defines two functions, @leftf@ and @rightf@, which should 
-- produce different functorial values.
class LR f where
  leftf  :: a -> f a
  rightf :: a -> f a

instance LR Id where
  leftf  x = Id x
  rightf x = Id x

instance LRBase a => LR (K a) where
  leftf  _ = K leftb
  rightf _ = K rightb

instance LR Unit where
  leftf  _ = Unit
  rightf _ = Unit

instance (LR f, LR g) => LR (f :+: g)  where
  leftf  x = L (leftf x)
  rightf x = R (rightf x)

instance (LR f, LR g) => LR (f :*: g)  where
  leftf  x = leftf x :*: leftf x
  rightf x = rightf x :*: rightf x

instance LR f => LR (Con f) where
  leftf  x = Con (error "Should never be inspected") (leftf x)
  rightf x = Con (error "Should never be inspected") (rightf x)

-- | Produces a value which should be different from the value returned by 
-- @right@.
left :: (Regular a, LR (PF a)) => a
left = to (leftf left)

-- | Produces a value which should be different from the value returned by 
-- @left@.
right :: (Regular a, LR (PF a)) => a
right = to (rightf right)
