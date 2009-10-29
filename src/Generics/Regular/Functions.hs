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
-- Generic show ("Generics.Regular.Functions.Show"), generic read 
-- ("Generics.Regular.Functions.Read") and generic equality 
-- ("Generics.Regular.Functions.Eq") are not exported to prevent clashes
-- with @Prelude@.
-----------------------------------------------------------------------------

module Generics.Regular.Functions (
  
    -- * Constructor names
    module Generics.Regular.Functions.ConNames,
    
    -- * Crush
    module Generics.Regular.Functions.Crush,
    
    -- * Generic folding
    module Generics.Regular.Functions.Fold,
    
    -- * Functorial map
    module Generics.Regular.Functions.GMap,
    
    -- * Generating values that are different on top-level
    module Generics.Regular.Functions.LR,
    
    -- * Deep seq
    module Generics.Regular.Functions.Seq,
    
    -- * Zipping
    module Generics.Regular.Functions.Zip

  ) where

import Generics.Regular.Functions.ConNames
import Generics.Regular.Functions.Crush
import Generics.Regular.Functions.Fold
import Generics.Regular.Functions.GMap
import Generics.Regular.Functions.LR
import Generics.Regular.Functions.Seq
import Generics.Regular.Functions.Zip
