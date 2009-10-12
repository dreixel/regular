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
-- Summary: Generic functionality for regular dataypes: mapM, flatten, zip,
-- equality, show, value generation and fold.
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
  
  -- * Read
  module Generics.Regular.Functions.Read,
  
  -- * Show
  module Generics.Regular.Functions.Show,
  
  -- * Zipping
  module Generics.Regular.Functions.Zip

) where

import Generics.Regular.Functions.Crush
import Generics.Regular.Functions.Eq
import Generics.Regular.Functions.Fold
import Generics.Regular.Functions.GMap
import Generics.Regular.Functions.LR
import Generics.Regular.Functions.Read
import Generics.Regular.Functions.Show
import Generics.Regular.Functions.Zip
