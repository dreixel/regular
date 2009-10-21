{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.HShow
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic show. This module is not exported by 
-- "Generics.Regular.Functions" to avoid clashes with "Prelude".
-----------------------------------------------------------------------------

module Generics.Regular.Functions.Show (

  -- * HShow function
  HShow (..),
  show, shows

) where

import Generics.Regular.Base
import Prelude hiding (Show, show, shows, showsPrec)
import qualified Prelude as P (Show, showsPrec)


-----------------------------------------------------------------------------
-- HShow function.
-----------------------------------------------------------------------------

-- | The @HShow@ class defines a show on values.
class HShow f where
  hshowsPrec :: (Int -> a -> ShowS) -> Bool -> Int -> f a -> ShowS

instance HShow I where
  hshowsPrec f _ n (I r) = f n r

instance (P.Show a) => HShow (K a) where
  hshowsPrec _ _ n (K x) = P.showsPrec n x

instance HShow U where
  hshowsPrec _ _ n U = id

instance (HShow f, HShow g) => HShow (f :+: g) where
  hshowsPrec f b n (L x) = hshowsPrec f b n x
  hshowsPrec f b n (R x) = hshowsPrec f b n x

instance (HShow f, HShow g) => HShow (f :*: g) where
  hshowsPrec f b n (x :*: y) = hshowsPrec f b n x 
                             . (if b then showString ", " else showString " ")
                             . hshowsPrec f b n y

instance (Constructor c, HShow f) => HShow (C c f) where
  hshowsPrec f _ n cx@(C x) = case fixity of
    Prefix -> showParen True (showString (conName cx) . showChar ' '                              . showBraces isRecord (hshowsPrec f isRecord n x))
    Infix _ n' -> showParen True 
                    (showChar '(' . showString (conName cx) 
                     . showChar ')' . showChar ' ' 
                     . showBraces isRecord (hshowsPrec f isRecord n x))
    where isRecord = conIsRecord cx
          fixity   = conFixity cx

showBraces       :: Bool -> ShowS -> ShowS
showBraces b p   =  if b then showChar '{' . p . showChar '}' else p

instance (Selector s, HShow f) => HShow (S s f) where
  hshowsPrec f b n s@(S x) = showString (selName s) . showString " = " 
                           . hshowsPrec f b n x


showsPrec :: (Regular a, HShow (PF a)) => Int -> a -> ShowS
showsPrec n x = hshowsPrec showsPrec False n (from x)

shows :: (Regular a, HShow (PF a)) => a -> ShowS
shows = showsPrec 0

show :: (Regular a, HShow (PF a)) => a -> String
show x = shows x ""
