{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Functions.ConNames
-- Copyright   :  (c) 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Return the name of all the constructors of a type.
--
-----------------------------------------------------------------------------

module Generics.Regular.Functions.ConNames (

    -- * Functionality for retrieving the names of all the possible contructors
    --   of a type
    ConNames(..), conNames

  ) where

import Generics.Regular.Base

class ConNames f where 
    hconNames :: f a -> [String]

instance (ConNames f, ConNames g) => ConNames (f :+: g) where
    hconNames (_ :: (f :+: g) a) = hconNames (undefined :: f a) ++
                                   hconNames (undefined :: g a)
    
instance (ConNames f, Constructor c) => ConNames (C c f) where
    hconNames (x :: (C c f) a) = [conName x]

instance ConNames (S s f) where
    hconNames _ = []

instance (ConNames f, ConNames g) => ConNames (f :*: g) where
    hconNames _ = []

instance ConNames I where
    hconNames _ = []

instance ConNames U where
    hconNames _ = []

instance ConNames (K a) where
    hconNames _ = []

-- | Return the name of all the constructors of the type of the given term.
conNames :: (Regular a, ConNames (PF a)) => a -> [String]
conNames x = hconNames (undefined `asTypeOf` (from x))

-------------------------------------------------------------------------------- 