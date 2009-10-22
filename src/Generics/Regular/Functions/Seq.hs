{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Seq
-- Copyright   :  (c) 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Deep generic seq. Used to fully evaluate a term.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Seq (

    DeepSeq (..), GSeq(..), gdseq
    
  ) where

import Data.List
import Generics.Regular.Base

-- | The class for generic deep seq.
class GSeq f where
  gseq :: (a -> b -> b) -> f a -> b -> b

instance GSeq I where
  gseq f (I x) = f x

-- | For constants we rely on the |DeepSeq| class.
instance (DeepSeq a) => GSeq (K a) where
  gseq _ (K x) = dseq x
  
instance GSeq U where
  gseq _ U = id

instance (GSeq f, GSeq g) => GSeq (f :+: g) where
  gseq f (L x) = gseq f x
  gseq f (R y) = gseq f y

instance (GSeq f, GSeq g) => GSeq (f :*: g) where
  gseq f (x :*: y) = gseq f x . gseq f y

instance GSeq f => GSeq (C c f) where
  gseq f (C x) = gseq f x

instance GSeq f => GSeq (S s f) where
  gseq f (S x) = gseq f x

-- | Deep, generic version of seq.

gdseq :: (Regular a, GSeq (PF a)) => a -> b -> b
gdseq p = gseq gdseq (from p)

-- | A general class for expressing deep seq. It is used in the 'K' case for
-- the generic seq.
--
-- We do not give an instance of the form
-- @instance (Regular a, GSeq (PF a)) => DeepSeq a where dseq = gdseq@
-- because this requires undecidable instances. However, any type for which
-- there is a generic instance can be given a trivial instance of 'DeepSeq' by
-- using 'gdseq'.
class DeepSeq a where
  dseq   :: a -> b -> b
  dseq = seq

instance DeepSeq Int
instance DeepSeq Integer
instance DeepSeq Char
instance DeepSeq Float
instance DeepSeq Double
instance DeepSeq ()

instance DeepSeq a => DeepSeq [a] where
  dseq xs b = foldl' (flip dseq) b xs

instance DeepSeq a => DeepSeq (Maybe a) where
  dseq Nothing  b = b
  dseq (Just a) b = dseq a b

instance (DeepSeq a, DeepSeq b) => DeepSeq (Either a b) where
  dseq (Left  x) b = dseq x b
  dseq (Right x) b = dseq x b
