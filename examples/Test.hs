{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE EmptyDataDecls     #-}

module Test where

import Generics.Regular

-- * Datatype representing logical expressions
data Logic = Var String
           | Logic :->:  Logic  -- implication
           | Logic :<->: Logic  -- equivalence
           | Logic :&&:  Logic  -- and (conjunction)
           | Logic :||:  Logic  -- or (disjunction)
           | Not Logic          -- not
           | T                  -- true
           | F                  -- false
           deriving Show

-- * Instantiating Regular for Logic using TH
-- ** Constructors
$(deriveConstructors ''Logic)

-- ** Functor encoding and 'Regular' instance
$(deriveRegular ''Logic "PFLogic")
type instance PF Logic = PFLogic

-- * Example logical expressions
l1, l2, l3 :: Logic
l1 = Var "p"
l2 = Not l1
l3 = l1 :->: l2

-- * Testing flattening
ex0 :: [Logic]
ex0 = flatten (from l3)

-- * Testing generic equality
ex1, ex2 :: Bool
ex1 = geq l3 l3
ex2 = geq l3 l2

-- * Testing generic show
ex3 :: String
ex3 = gshow l3 ""

-- * Testing value generation
ex4, ex5 :: Logic
ex4 = left
ex5 = right

-- * Testing folding
ex6 :: (String -> Bool) -> Logic -> Bool
ex6 env = fold (env & impl & (==) & (&&) & (||) & not & True & False)
  where impl p q = not p || q

ex7 :: Bool
ex7 = ex6 (\_ -> False) l3
