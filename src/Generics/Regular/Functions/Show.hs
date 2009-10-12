{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.Show
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic show.
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Show (

  -- * Show function
  Show (..),
  show, shows

) where

import Generics.Regular.Base
import Prelude hiding (Show, show, shows)
import qualified Prelude as P (Show, shows)


-----------------------------------------------------------------------------
-- Show function.
-----------------------------------------------------------------------------

-- | The @Show@ class defines a show on values.
class Show f where
  showf :: (a -> ShowS) -> Bool -> f a -> ShowS

instance Show I where
  showf f _ (I r) = f r

instance (P.Show a) => Show (K a) where
  showf _ _ (K x) = P.shows x

instance Show U where
  showf _ _ U = id

instance (Show f, Show g) => Show (f :+: g) where
  showf f r (L x) = showf f r x
  showf f r (R x) = showf f r x

instance (Show f, Show g) => Show (f :*: g) where
  showf f r (x :*: y) = showf f r x . showString (if r then ", " else " ") 
                       . showf f r y

instance (Constructor c, Show f) => Show (C c f) where
  showf f _ cx@(C x) = 
    showParen True (showString (conName cx) . showChar ' ' 
                   . showBraces isRecord (showf f isRecord x))
      where isRecord = conIsRecord cx

showBraces       :: Bool -> ShowS -> ShowS
showBraces b p   =  if b then showChar '{' . p . showChar '}' else p

instance (Selector s, Show f) => Show (S s f) where
  showf f r s@(S x) = showString (selName s) . showString " = " . showf f r x


shows :: (Regular a, Show (PF a)) => a -> ShowS
shows x = showf shows False (from x)

show :: (Regular a, Show (PF a)) => a -> String
show x = shows x ""
