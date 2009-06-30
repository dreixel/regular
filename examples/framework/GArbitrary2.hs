{-# OPTIONS -fglasgow-exts #-}

module GArbitrary2 where

import Prelude hiding (sum)
import qualified Prelude
import Control.Monad
import Control.Applicative
import System.Random
import Test.QuickCheck
import Debug.Trace

import Generics.Regular

Just x `combM` Just y = Just (x+y)
Just x `combM` _      = Just x
_      `combM` Just y = Just y
Nothing `combM` Nothing = Nothing

instance Applicative (PFAnalysis e) where
  pure x = PFAnalysis [(Nothing,const (return x))]
  (PFAnalysis xs) <*> (PFAnalysis ys)
    = PFAnalysis [(mx `combM` my,\e -> (x e) `ap` (y e))| (mx,x) <- xs, (my,y) <- ys]

instance Alternative (PFAnalysis e) where
  empty = PFAnalysis []
  (PFAnalysis xs) <|> (PFAnalysis ys) = PFAnalysis (xs ++ ys)

data PFAnalysis a b = PFAnalysis [(Maybe Int,Gen a -> Gen b)]

instance Functor (PFAnalysis a) where
  fmap f (PFAnalysis xs) = PFAnalysis [(mx,fmap f . x)|(mx,x)<-xs]

class GArbitrary f where
  garbitraryf :: PFAnalysis a (f a)

instance GArbitrary U where
  garbitraryf = pure U

instance GArbitrary I where
  garbitraryf = PFAnalysis [(Just 1,\e -> liftM I e)]

instance Arbitrary a => GArbitrary (K a) where
  garbitraryf = PFAnalysis [(Nothing,const $ liftM K arbitrary)]

instance (GArbitrary f,GArbitrary g) => GArbitrary (f :*: g) where
  garbitraryf = (:*:) <$> garbitraryf <*> garbitraryf

instance (GArbitrary f,GArbitrary g) => GArbitrary (f :+: g) where
  garbitraryf = L <$> garbitraryf <|> R <$> garbitraryf

garbitrary :: (Regular a, GArbitrary (PF a)) => Int -> Gen a
garbitrary = go
  where
    go 0  = frequency nonRec
    go sz = frequency (all sz)

    PFAnalysis cons = fmap to garbitraryf

    nonRec = [(1,f undefined)  | (Nothing,f) <- cons]
    all sz = nonRec ++
             [(fixps+1,f $ go $ sz `div` fixps) | (Just fixps,f) <- cons]

