import Generics.Regular
import GSmall
import GArbitrary
import Types

import System.Random
import Test.QuickCheck
import Data.List

genDirection :: StdGen -> Direction ()
genDirection gen = generate 80 gen (sized garbitrary)

dirIO = newStdGen >>= (return . genDirection)

genTree20 :: StdGen -> Tree Int
genTree20 gen = generate 80 gen (sized garbitrary)

treeIO = newStdGen >>= (return . num . gsize . genTree20)

-- k s = do
--   ks <- mapM (const dirIO) [1..s]
--   print $ ("Avg: ", sum ks `div` s)
--   print $ ("Min: ", minimum ks)
--   print $ ("Max: ", maximum ks)

dirTest s = 
  mapM (const dirIO) [1..s] >>=
  mapM_ (\x -> print (length x, head x)) . group . sort

-- generates an infinite value (!)
infiniteTest :: IO (Rec ())
infiniteTest = newStdGen >>= (\g -> return $ generate 1 g (sized garbitrary))

mygen gen = generate 80 gen $ oneof (map return [North,NorthEast,East,SouthEast,South,SouthWest,West,NorthWest])

dirIO2 = newStdGen >>= (return . mygen)

dirTest2  s = 
  mapM (const dirIO2) [1..s] >>=
  mapM_ (\x -> print (length x, head x)) . group . sort

genA :: StdGen -> A
genA gen = generate 80 gen (sized garbitrary)
aIO = newStdGen >>= (return . genA)
aTest s = 
  mapM (const aIO) [1..s] >>=
  mapM_ (\x -> print (length x, head x)) . group . sort

