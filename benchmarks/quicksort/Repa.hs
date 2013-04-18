module Repa (quicksortR) where

import Data.Array.Repa as R
import System.IO.Unsafe

-- NB: This version doesn't actually seem to be nested parallel. All ls, es and gs are evaluated recursively sequentially and THEN the append seems to happen.

quicksortR:: Array U DIM1 Double -> Array U DIM1 Double 
quicksortR xs = unsafePerformIO $ computeP $ qsortVect' (delay xs)
 
qsortVect':: Array D DIM1 Double -> Array D DIM1 Double
qsortVect' xs | (size (extent xs)) == 0 = xs
              | otherwise =
  let p  = linearIndex xs ((size (extent xs)) `div` 2)
      ls = unsafePerformIO $ R.selectP (\idx -> (linearIndex xs idx) < p) (\idx -> linearIndex xs idx) (size (extent xs))
      gs = unsafePerformIO $ R.selectP (\idx -> (linearIndex xs idx) > p) (\idx -> linearIndex xs idx) (size (extent xs))
      es = unsafePerformIO $ R.selectP (\idx -> (linearIndex xs idx) == p) (\idx -> linearIndex xs idx) (size (extent xs))
  in
  qsortVect' (delay ls) R.++ es R.++ qsortVect' (delay gs) 
