import BGen
import Data.List
import System.Random

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
    let n100 = randomList 100 seed
    let n1000 = randomList 1000 seed
    let n10000 = randomList 10000 seed
    let a = quickSortDT n100
    let b = quickSortDT n1000
    let c = quickSortDT n10000
    let x = quickSortST n100
    let y = quickSortST n1000
    let z = quickSortST n10000
    putStrLn "Finished"
