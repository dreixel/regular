{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Fold
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic folding.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Fold (

  -- * Generic folding
  Alg, Algebra,
  Fold, alg,
  fold,
  
  CoAlg, CoAlgebra,
  Unfold, coalg,
  unfold,
  
  (&)  

) where

import Generics.Regular.Base


-----------------------------------------------------------------------------
-- Folds
-----------------------------------------------------------------------------

type family Alg (f :: (* -> *)) 
                (r :: *) -- result type
                :: *

-- | For a constant, we take the constant value to a result.
type instance Alg (K a) r = a -> r

-- | For a unit, no arguments are available.
type instance Alg U r = r

-- | For an identity, we turn the recursive result into a final result.
type instance Alg I r = r -> r

-- | For a sum, the algebra is a pair of two algebras.
type instance Alg (f :+: g) r = (Alg f r, Alg g r)

-- | For a product where the left hand side is a constant, we
--   take the value as an additional argument.
type instance Alg (     K a  :*: g) r = a -> Alg g r
type instance Alg (S s (K a) :*: g) r = a -> Alg g r

-- | For a product where the left hand side is an identity, we
--   take the recursive result as an additional argument.
type instance Alg (I :*: g) r = r -> Alg g r

-- | Constructors are ignored.
type instance Alg (C c f) r = Alg f r

-- | Selectors are ignored.
type instance Alg (S s f) r = Alg f r


type Algebra a r = Alg (PF a) r

-- | The class fold explains how to convert an algebra
--   'Alg' into a function from functor to result.
class Fold (f :: * -> *) where
  alg :: Alg f r -> f r -> r

instance Fold (K a) where
  alg f (K x) = f x

instance Fold U where
  alg f U     = f

instance Fold I where
  alg f (I x) = f x

instance (Fold f, Fold g) => Fold (f :+: g) where
  alg (f, _) (L x) = alg f x
  alg (_, g) (R x) = alg g x

instance (Fold g) => Fold (K a :*: g) where
  alg f (K x :*: y) = alg (f x) y

instance (Fold g) => Fold (I :*: g) where
  alg f (I x :*: y) = alg (f x) y

instance (Fold f) => Fold (C c f) where
  alg f (C x) = alg f x

instance (Fold f) => Fold (S s f) where
  alg f (S x) = alg f x

-- | Fold with convenient algebras.
fold :: (Regular a, Fold (PF a), Functor (PF a))
     => Algebra a r -> a -> r
fold f = alg f . fmap (\x -> fold f x) . from

-----------------------------------------------------------------------------
-- Unfolds
-----------------------------------------------------------------------------

type family CoAlg (f :: (* -> *)) 
                  (s :: *) -- seed type
                  :: *

-- | For a constant, we produce a constant value as a result.
type instance CoAlg (K a) s = a

-- | For an identity, we produce a new seed to create the recursive result.
type instance CoAlg I s = s

-- | For a sum, the coalgebra produces either the left or the right side. When
-- one of the sides is a U we optimize this away by using a Maybe, similarly
-- to the specific 'Data.List.unfold' for []. These type instances for sum
-- automatically ignore the constructors.
type instance CoAlg (C c (f :*: g) :+: C d U)         s = Maybe  (CoAlg (f :*: g) s)
type instance CoAlg (C c U         :+: C d (f :*: g)) s = Maybe  (CoAlg (f :*: g) s)
type instance CoAlg (C c (e :*: f) :+: C d (g :*: h)) s = Either (CoAlg (e :*: f) s) (CoAlg (g :*: h) s)

-- | For a produt, the coalgebra is a pair of the two arms.
type instance CoAlg (f :*: g) s = (CoAlg f s, CoAlg g s)

-- | Constructors are ignored.
type instance CoAlg (C c f) s = CoAlg f s

-- | Selectors are ignored.
type instance CoAlg (S r f) s = CoAlg f s

type CoAlgebra a s = s -> CoAlg (PF a) s

-- | The class unfold explains how to convert a coalgebra 'CoAlg' and a seed
-- into a representation.
class Unfold (f :: * -> *) where
  coalg :: (s -> a) -> CoAlg f s -> f a

instance Unfold (K a) where
  coalg _ = K

instance Unfold I where
  coalg r a = I (r a)

instance (Unfold f, Unfold g) => Unfold (C c (f :*: g) :+: C d U) where
  coalg r (Just  c) = L (C (coalg r c))
  coalg _ Nothing   = R (C U)

instance (Unfold f, Unfold g) => Unfold (C c U :+: C d (f :*: g)) where
  coalg _ Nothing   = L (C U)
  coalg r (Just  c) = R (C (coalg r c))

instance (Unfold e, Unfold f, Unfold g, Unfold h) => Unfold (C c (e :*: f) :+: C d (g :*: h)) where
  coalg r (Left  c) = L (C (coalg r c))
  coalg r (Right c) = R (C (coalg r c))

instance (Unfold f, Unfold g) => Unfold (f :*: g) where
  coalg r (c, g) = coalg r c :*: coalg r g

instance Unfold f => Unfold (S s f) where
  coalg r = S . coalg r

unfold :: (Unfold (PF a), Regular a) => CoAlgebra a s -> s -> a
unfold a = to . coalg (unfold a) . a

-----------------------------------------------------------------------------

-- Construction of algebras
infixr 5 &

-- | For constructing algebras it is helpful to use this pairing combinator.
(&) :: a -> b -> (a, b)
(&) = (,)

