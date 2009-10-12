{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Crush
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic crush.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Crush (

  -- * Crush functions
  Crush (..),
  flattenl, flattenr, crushr, crushl

) where

import Generics.Regular.Base


-----------------------------------------------------------------------------
-- Crush functions.
-----------------------------------------------------------------------------

-- | Associativity of the binary operator used for 'crush'
data Assoc = AssocLeft  -- ^ Left-associative
           | AssocRight -- ^ Right-associative


-- | The @Crush@ class defines a right-associative crush on functorial values.
class Crush f where
  crush :: Assoc -> (a -> b -> b) -> b -> f a -> b

instance Crush I where
  crush _ op e (I x) = x `op` e

instance Crush (K a) where
  crush _ _ e _ = e

instance Crush U where
  crush _ _ e _ = e

instance (Crush f, Crush g) => Crush (f :+: g) where
  crush asc op e (L x) = crush asc op e x
  crush asc op e (R y) = crush asc op e y

instance (Crush f, Crush g) => Crush (f :*: g) where
  crush asc@AssocRight op e (x :*: y) = crush asc op (crush asc op e y) x
  crush asc@AssocLeft  op e (x :*: y) = crush asc op (crush asc op e x) y

instance Crush f => Crush (C c f) where
  crush asc op e (C x) = crush asc op e x

instance Crush f => Crush (S s f) where
  crush asc op e (S x) = crush asc op e x

-- | Flatten a structure by collecting all the elements present.
flattenr, flattenl :: Crush f => f a -> [a]
flattenr = crushr (:) []
flattenl = crushl (:) []

crushr, crushl :: Crush f => (a -> b -> b) -> b -> f a -> b
crushr = crush AssocRight
crushl = crush AssocLeft
