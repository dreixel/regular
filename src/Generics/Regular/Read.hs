{-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE Rank2Types            #-}
-- {-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Read
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic read.
-----------------------------------------------------------------------------

module Generics.Regular.Read (
    read, readPrec, readsPrec
) where

-----------------------------------------------------------------------------
-- Generic read.
-----------------------------------------------------------------------------

import Generics.Regular.Base

import Data.Char
import Control.Monad
import Text.Read hiding (readsPrec, readPrec, read)
import Prelude hiding (readsPrec, read)
import qualified Prelude as P (readsPrec)
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec

-- Count the number of terms in a product

class CountAtoms f where 
  countatoms :: f r -> Int

instance CountAtoms (K a) where
  countatoms _ = 1

instance CountAtoms I where
  countatoms _ = 1

instance (CountAtoms f, CountAtoms g) => CountAtoms (f :*: g) where
  countatoms (_ :: (f :*: g) r) = countatoms (undefined :: f r) + countatoms (undefined :: g r)

-- * Generic read

class HReadPrec f where
   hreader :: ReadPrec a -> ReadPrec (f a)


instance HReadPrec U where
   hreader _ = return U

instance (Read a) => HReadPrec (K a) where
   hreader _ = liftM K (readS_to_Prec P.readsPrec)

instance HReadPrec I where
   hreader f = liftM I f

instance (HReadPrec f, HReadPrec g) => HReadPrec (f :+: g) where
   hreader f = liftM L (hreader f) +++ liftM R (hreader f)

instance (HReadPrec f, HReadPrec g) => HReadPrec (f :*: g) where
   hreader f = liftM2 (:*:) (hreader f) (hreader f)


 -- Dealing with constructors

-- No arguments
instance (Constructor c) => HReadPrec (C c U) where
   hreader f = let constr = undefined :: C c U r
                   name   = conName constr
               in readCons (readNoArgsCons f name)

-- 1 argument
instance (Constructor c, HReadPrec I) => HReadPrec (C c I) where
   hreader f = let constr = undefined :: C c I r
                   name   = conName constr
               in  readCons (readPrefixCons f True name)

instance (Constructor c, HReadPrec (K a)) => HReadPrec (C c (K a)) where
   hreader f = let constr = undefined :: C c (K a) r
                   name   = conName constr
               in  readCons (readPrefixCons f True name) 

-- 2 arguments or more
instance (Constructor c, CountAtoms (f :*: g), HReadPrec f, HReadPrec g) 
         => HReadPrec (C c (f:*:g)) where
   hreader f = let constr = undefined :: C c (f:*:g) r
                   name   = conName constr
                   fixity = conFixity constr
                   (assoc,prc,isInfix) = case fixity of 
                                           Prefix    -> (LeftAssociative, 9, False)
                                           Infix a p -> (a, p, True)
                   nargs  = countatoms (undefined :: (f :*: g) r)
               in   readCons $  
                               readPrefixCons f (not isInfix) name
                                        +++
                               (do guard (nargs == 2)
                                   readInfixCons f (assoc,prc,isInfix) name
                               )


readCons :: (Constructor c) => ReadPrec (f a) -> ReadPrec (C c f a)
readCons = liftM C

readPrefixCons :: (HReadPrec f) 
               => ReadPrec a -> Bool -> String -> ReadPrec (f a)
readPrefixCons f b name = parens . prec appPrec $
                            do parens (prefixConsNm name b) 
                               step (hreader f)
    where prefixConsNm s True  = do Ident n <- lexP
                                    guard (s == n)
          prefixConsNm s False = do Punc "(" <-lexP
                                    Symbol n <- lexP
                                    guard (s==n)   
                                    Punc ")" <- lexP
                                    return ()


readInfixCons :: (HReadPrec f, HReadPrec g)
              => ReadPrec a -> (Associativity,Int,Bool) -> String -> ReadPrec ((f :*: g) a)
readInfixCons f (asc,prc,b) name = parens . prec prc $
                                       do x <- {- (if asc == LeftAssociative  then id else step) -} step (hreader f)
                                          parens (infixConsNm name b)
                                          y <- (if asc == RightAssociative then id else step) (hreader f)
                                          return  (x :*: y)
     where  infixConsNm s True  = do Symbol n <- lexP 
                                     guard (n == s) 
            infixConsNm s False = do Punc "`"  <- lexP
                                     Ident n   <- lexP  
                                     guard (n == s)
                                     Punc "`"  <- lexP 
                                     return ()

readNoArgsCons :: ReadPrec a -> String -> ReadPrec (U a)
readNoArgsCons _ name = parens $ 
                             do Ident n <- lexP
                                guard (n == name)
                                return U

appPrec :: Prec
appPrec = 10


-- Exported functions

readPrec :: (Regular a, HReadPrec (PF a)) => ReadPrec a
readPrec = liftM to (hreader readPrec)

readsPrec :: (Regular a, HReadPrec (PF a)) => Int -> ReadS a
readsPrec n = readPrec_to_S readPrec n

read :: (Regular a, HReadPrec (PF a)) => String -> a
read s = case [x |  (x,remain) <- readsPrec 0 s , all isSpace remain] of
           [x] -> x 
           [ ] -> error "no parse"
           _   -> error "ambiguous parse"
