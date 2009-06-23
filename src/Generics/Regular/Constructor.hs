{-# LANGUAGE KindSignatures       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Constructor
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Representation for constructors.
-----------------------------------------------------------------------------

module Generics.Regular.Constructor (
    Constructor(..), Fixity(..), Associativity(..)
  ) where


-- | Class for datatypes that represent data constructors.
-- For non-symbolic constructors, only 'conName' has to be defined.
-- The weird argument is supposed to be instantiated with 'C' from
-- base, hence the complex kind.
class Constructor c where
  conName   :: t c (f :: * -> *) r -> String
  conFixity :: t c (f :: * -> *) r -> Fixity
  conFixity = const Prefix

-- | Datatype to represent the fixity of a constructor. An infix declaration
-- directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

data Associativity = LeftAssociative | RightAssociative | NotAssociative
  deriving (Eq, Show, Ord, Read)
