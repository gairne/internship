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

myPartition :: Ord a => a -> [a] -> ([a], [a]) -> ([a], [a])
myPartition _ [] (l,h) = (l,h)
myPartition p (x:xs) (l,h)
    | x > p = myPartition p xs (l , x:h)
    | x <= p = myPartition p xs (x:l, h)
    | otherwise = (l,h)

main = do
    seed <- newStdGen
    let n1000 = randomList 1000 seed
    let n10000 = randomList 10000 seed
    defaultMain [
             bgroup "quickSort"
               [ bench "qs single 1000" $ whnf quickSortST n1000
               , bench "qs single 10000" $ whnf quickSortST n10000
               , bench "qs dual 1000" $ whnf quickSortDT n1000
               , bench "qs dual 10000" $ whnf quickSortDT n10000
               ]
           ]
 
