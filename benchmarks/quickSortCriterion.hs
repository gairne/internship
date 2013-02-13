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
quickSortST (p:xs) = quickSortST (fst split) ++ [p] ++ quickSortST (snd split)
  where
  split = partition (<p) xs

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
 
