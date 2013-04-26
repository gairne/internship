{-# LANGUAGE ScopedTypeVariables #-}

import Randomish

import Vectorised
import Data.Array.Parallel
--import qualified Data.Vector                    as V
--import qualified Vector                         as V
import Control.Exception                        (evaluate)
--import qualified Data.Vector.Unboxed            as U
--import Data.Array.Parallel.PArray               as PA hiding (length)

import Timing
import System.IO
import Repa



percentElements = 0.25

sparseRow :: Int -> Int ->[(Int, Double)]
sparseRow cols seed = filter (/= (0,0)) (map (\i -> if (randoms !! i) < percentElements then (i, value !! i) else (0,0)) [0..cols-1])
  where
    randoms = (randomishDoubles cols 0 1 seed)
    value = (randomishDoubles cols 0 100 seed+1)

sparseMatrix :: Int -> Int -> ([(Int, Double)], [Int], [Int])
sparseMatrix rows cols = (matrix, lengths matrix, indicies matrix)
  where
    matrix = map (sparseRow cols) [0..rows-1]
    indicies matrix = init (0 : (scanl1 (+) (lengths matrix)))
    lengths matrix = map length matrix
 
main = putStrLn(show (sparseMatrix 10 10))


