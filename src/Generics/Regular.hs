-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Rewriting.Representations
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Top-level module for this library.
-- By importing this module, the user is able to use all the generic
-- functionality. The user is only required to provide an instance of
-- @Regular@ for the datatype.
--
-- Consider a datatype representing logical propositions:
--
-- @
--   data Expr = Const Int | Expr :++: Expr | Expr :**: Expr deriving Show
-- @
--
-- An instance of @Regular@ would look like:
--
-- @
--   instance Regular Expr where
--     type PF Expr = Con (K Int) :+: Con (Id :*: Id) :+: Con (Id :*: Id)
--     from (Const n)    = L (Con \"Const\" (K n))
--     from (e1 :++: e2) = R (L (Con \"(:++:)\" $ (Id e1) :*: (Id e2)))
--     from (e1 :**: e2) = R (R (Con \"(:**:)\" $ (Id e1) :*: (Id e2)))
--     to (L (Con _ (K n)))                        = Const n
--     to (R (L (Con _ ((Id r1) :*: (Id r2))))) = r1 :++: r2
--     to (R (R (Con _ ((Id r1) :*: (Id r2))))) = r1 :**: r2
-- @
-----------------------------------------------------------------------------

module Generics.Regular (
    module Generics.Regular.Base,
    module Generics.Regular.Functions
  ) where

import Generics.Regular.Base
import Generics.Regular.Functions
