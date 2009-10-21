{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE EmptyDataDecls     #-}

module Test where

import Generics.Regular
import Generics.Regular.Functions
import qualified Generics.Regular.Functions.Show as G
import qualified Generics.Regular.Functions.Read as G


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
ex0 = flattenr (from l3)

-- * Testing generic equality
ex1, ex2 :: Bool
ex1 = eq l3 l3
ex2 = eq l3 l2

-- * Testing generic show
ex3 :: String
ex3 = G.show l3

-- * Testing generic read
ex4 :: Logic
ex4 = G.read ex3

-- * Testing value generation
ex5, ex6 :: Logic
ex5 = left
ex6 = right

-- * Testing folding
ex7 :: Bool
ex7 = fold (alg (\_ -> False)) l3 where
  alg env = (env & impl & (==) & (&&) & (||) & not & True & False)
  impl p q = not p || q

-- * Testing unfolding
ex8 :: Int -> Logic
ex8 n = unfold alg n where
  alg :: CoAlgebra Logic Int
  alg n | odd n || n <= 0 = Left ""
        | even n          = Right (Left (n-1,n-2))
