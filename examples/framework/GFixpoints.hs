{-# OPTIONS -fglasgow-exts #-}

module GFixpoints where

import Prelude
import Control.Applicative

import Generics.Regular

-- This assumes we have sums of products (with NO nested sums within the products)

data Tree a = Leaf a | Node (Tree a) (Tree a)
 deriving Show

instance Applicative Tree where
  pure = Leaf
  Leaf x <*> Leaf y = Leaf (x y)
  -- partial instance

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree l n (Leaf x)    = l x
foldTree l n (Node x y)  = (foldTree l n x) `n` (foldTree l n y)

sum :: Tree Int -> Int
sum = foldTree id (+)

instance Functor Tree where
  fmap f = foldTree (Leaf . f) Node

class GFixpoints f where
  gfixpoints' :: f a -> Tree Int

instance GFixpoints U where
  gfixpoints' _ = Leaf 0

instance GFixpoints I where
  gfixpoints' _ = Leaf 1

instance GFixpoints (K a) where
  gfixpoints' _ = Leaf 0

instance (GFixpoints f, GFixpoints g) => GFixpoints (f :+: g) where
  gfixpoints' _ = gfixpoints' (undefined :: f a)
           `Node` gfixpoints' (undefined :: g a)

instance (GFixpoints f, GFixpoints g) => GFixpoints (f :*: g) where
  gfixpoints' _ = (+) <$> gfixpoints' (undefined :: f a)
                      <*> gfixpoints' (undefined :: g a)

