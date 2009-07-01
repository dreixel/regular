{-# OPTIONS -fglasgow-exts #-}

module Types where

import Control.Applicative
import Test.QuickCheck

import Generics.Regular
import GFixpoints hiding (Tree, Leaf, Node)

-- Regular instance for the list data type.

type instance PF [a] = U :+: ((K a) :*: I)
instance Regular [a] where
  from []       = L U
  from (x : xs) = R ((K x) :*: (I (xs)))

  to (L U)                = []
  to (R ((K x) :*: (I r))) = x : r

-- Tree data type definition.

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Eq, Ord)

-- Regular instance for the tree data type.

type instance PF (Tree a) = (K a) :*: I :*: I :+: U
instance Regular (Tree a) where
  to (R U)                          = Leaf
  to (L ((K a) :*: (I l) :*: (I r))) = Branch a (l) (r)

  from Leaf           = R U
  from (Branch a l r) = L $ (K a) :*: (I l) :*: (I r)

-- Regular instance for the cardinal direction data type.

data Direction a =
    North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Eq, Show, Ord)

type instance PF (Direction a) =
    U :+: U :+: U :+: U :+: U :+: U :+: U :+: U
instance Regular (Direction a) where
  to (L U)                         = North     
  to (R (L U))                     = NorthEast
  to (R (R (L U)))                 = East
  to (R (R (R (L U))))             = SouthEast
  to (R (R (R (R (L U)))))         = South
  to (R (R (R (R (R (L U))))))     = SouthWest
  to (R (R (R (R (R (R (L U))))))) = West
  to (R (R (R (R (R (R (R U))))))) = NorthWest

  from North     = L U
  from NorthEast = R (L U)
  from East      = R (R (L U))
  from SouthEast = R (R (R (L U)))
  from South     = R (R (R (R (L U))))
  from SouthWest = R (R (R (R (R (L U)))))
  from West      = R (R (R (R (R (R (L U))))))
  from NorthWest = R (R (R (R (R (R (R U))))))

-- Regular instance for the recursive identity.

data Rec a = Rec (Rec a)
  deriving (Show, Eq, Ord)

type instance PF (Rec a) = I
instance Regular (Rec a) where
  to (I a) = Rec a
  from (Rec a) = I (a)

-- balanced to the left
data A = A A | B A | C_
 deriving (Show, Eq, Ord)

type instance PF A = U :+: I :+: I
instance Regular A where
  

  to (R (L (I x))) = A (x)
  to (R (R (I x))) = B (x)
  to (L U)       = C_
  from (A x) = (R (L (I x)))
  from (B x) = (R (R (I x)))
  from C_ = (L U)
