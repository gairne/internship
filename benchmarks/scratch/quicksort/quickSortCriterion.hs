import BGen
import Data.List
import System.Random
import Criterion.Main

-- quickSort with dual traversal (inefficient)
quickSortDT :: Ord a => [a] -> [a]
quickSortDT [] = []
quickSortDT (p:xs) = quickSortDT low ++ [p] ++ quickSortDT high
  where
  low = filter (<p) xs
  high = filter (>=p) xs

-- quickSort with single traversal
quickSortST :: Ord a => [a] -> [a]
quickSortST [] = []
quickSortST (p:xs) = quickSortST low ++ [p] ++ quickSortST high
  where
--partition call is slow!  (low, high) = partition (<p) xs
  (low,high) = myPartition p xs ([],[])

-- This menthod uses a partition library call rather then a handmade function like quickSortST
-- This seems slower, probably because of the function call use instead of pattern matching and guards.
quickSortPartition :: Ord a => [a] -> [a]
quickSortPartition [] = []
quickSortPartition(p:xs) = quickSortPartition low ++ [p] ++ quickSortPartition high
  where
  (low, high) = partition (<p) xs

myPartition :: Ord a => a -> [a] -> ([a], [a]) -> ([a], [a])
myPartition _ [] (l,h) = (l,h)
myPartition p (x:xs) (l,h)
    | x > p = myPartition p xs (l , x:h)
    | x <= p = myPartition p xs (x:l, h)

main = do
    seed <- newStdGen
    let n1000 = randomList 1000 seed
    let n10000 = randomList 10000 seed
    defaultMain [
             bgroup "quickSort"
               [ bench "qs single 1000" $ nf quickSortST n1000
               , bench "qs single 10000" $ nf quickSortST n10000
               , bench "qs dual 1000" $ nf quickSortDT n1000
               , bench "qs dual 10000" $ nf quickSortDT n10000
               , bench "qs part 1000" $ nf quickSortPartition n1000
               , bench "qs part 10000" $ nf quickSortPartition n10000
               ]
           ]
 
