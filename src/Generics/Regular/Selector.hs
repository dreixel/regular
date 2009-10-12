{-# LANGUAGE KindSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Selector
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Representation for record selectors.
-----------------------------------------------------------------------------

module Generics.Regular.Selector (Selector(..)) where

class Selector s where
  selName   :: t s (f :: * -> *) r -> String

