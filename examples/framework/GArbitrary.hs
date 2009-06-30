{-# OPTIONS -fglasgow-exts #-}

module GArbitrary where

import Prelude hiding (sum)
import qualified Prelude
import Control.Monad
import Control.Applicative
import System.Random
import Test.QuickCheck
import Debug.Trace

import Generics.Regular
import GFixpoints

-- Applicative and alternative instances for the random number generator.

instance Applicative Gen where
  (<*>) = ap
  pure  = return

instance Alternative Gen where
  a <|> b = oneof [a, b]
  empty   = oneof []

-- Generically create arbitrary instances for arbitrary data types.

data ArbParams a = AP {
    rec     :: ArbParams a -> Int -> Gen a
  , divisor :: Int
  }

class GArbitrary f where
  garbitrary' :: ArbParams a -> Int -> Gen (f a)

instance GArbitrary U where
  garbitrary' _ _ = pure U

instance GArbitrary I where
  garbitrary' p@(AP r d) t = I <$> r p (t `div` d)

instance Arbitrary a => GArbitrary (K a) where
  garbitrary' _ _ = K <$> arbitrary

instance (GFixpoints f, GFixpoints g,
          GArbitrary f, GArbitrary g) => GArbitrary (f :+: g) where
  garbitrary' p s = frequency [rec fpl L, rec fpr R]
    where rec a b = (fr a, b <$> garbitrary' p {divisor = sum a} s)
          (Node fpl fpr) = gfixpoints' (undefined :: (f :+: g) a)
          fr = foldTree (f s) (+)
          f 0 0 = 1 -- if our size (s) is zero, we only give non-recursive constructors a chance
          f 0 _ = 0
          f _ x = x + 1

instance (GArbitrary f, GArbitrary g) => GArbitrary (f :*: g) where
  garbitrary' p s = (:*:) <$> garbitrary' p s <*> garbitrary' p s

-- The first undefined gets filled in with garbitraryFix, the second in the instance for Sum.
garbitrary :: (Regular a, GArbitrary (PF a)) => Int -> Gen a
garbitrary s = garbitraryHelper (AP undefined 1) s

garbitraryHelper :: (Regular a, GArbitrary (PF a)) => ArbParams a -> Int -> Gen a
garbitraryHelper p s = to <$> garbitrary' p {rec = garbitraryHelper} s




