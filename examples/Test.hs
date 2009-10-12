{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE EmptyDataDecls     #-}

module Test where

import Generics.Regular
import qualified Generics.Regular.Functions as G
import Generics.Regular.Functions.Fold

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
$(deriveAll ''Logic "PFLogic")
type instance PF Logic = PFLogic

-- * Example logical expressions
l1, l2, l3 :: Logic
l1 = Var "p"
l2 = Not l1
l3 = l1 :->: l2

-- * Testing flattening
ex0 :: [Logic]
ex0 = G.flattenr (from l3)

-- * Testing generic equality
ex1, ex2 :: Bool
ex1 = G.eq l3 l3
ex2 = G.eq l3 l2

-- * Testing generic show
ex3 :: String
ex3 = G.show l3

-- * Testing generic read
ex4 :: Logic
ex4 = G.read ex3

-- * Testing value generation
ex5, ex6 :: Logic
ex5 = G.left
ex6 = G.right

-- * Testing folding
ex7 :: (String -> Bool) -> Logic -> Bool
ex7 env = G.fold (env & impl & (==) & (&&) & (||) & not & True & False)
  where impl p q = not p || q

ex8 :: Bool
ex8 = ex7 (\_ -> False) l3
