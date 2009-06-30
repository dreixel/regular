-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular
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
-- Consider a datatype representing logical expressions:
--
-- >  data Logic = Var String
-- >             | Logic :->:  Logic  -- implication
-- >             | Logic :<->: Logic  -- equivalence
-- >             | Logic :&&:  Logic  -- and (conjunction)
-- >             | Logic :||:  Logic  -- or (disjunction)
-- >             | Not Logic          -- not
-- >             | T                  -- true
-- >             | F                  -- false
--
-- An instance of @Regular@ is derived with TH by invoking:
--
-- >  $(deriveConstructors ''Logic)
-- >  $(deriveRegular ''Logic "PFLogic")
-- >  type instance PF Logic = PFLogic
-- 
-----------------------------------------------------------------------------

module Generics.Regular (
    module Generics.Regular.Base,
    module Generics.Regular.Functions,
    module Generics.Regular.TH
  ) where

import Generics.Regular.Base
import Generics.Regular.Functions
import Generics.Regular.TH
