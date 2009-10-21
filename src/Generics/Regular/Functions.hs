{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeFamilies      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: All of the generic functionality for regular dataypes: mapM, 
-- flatten, zip, equality, value generation, fold and unfold.
-- Generic show ("Generics.Regular.Functions.Show") and generic read 
-- ("Generics.Regular.Functions.Read") are not exported to prevent clashes
-- with @Prelude@.
-----------------------------------------------------------------------------

module Generics.Regular.Functions (
  
  -- * Crush
  module Generics.Regular.Functions.Crush,
  
  -- * Equality
  module Generics.Regular.Functions.Eq,
  
  -- * Generic folding
  module Generics.Regular.Functions.Fold,
  
  -- * Functorial map
  module Generics.Regular.Functions.GMap,
  
  -- * Generating values that are different on top-level
  module Generics.Regular.Functions.LR,
  
  -- * Zipping
  module Generics.Regular.Functions.Zip

) where

import Generics.Regular.Functions.Crush
import Generics.Regular.Functions.Eq
import Generics.Regular.Functions.Fold
import Generics.Regular.Functions.GMap
import Generics.Regular.Functions.LR
import Generics.Regular.Functions.Zip
