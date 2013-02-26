module Repa (quicksortR) where
import Data.Array.Repa as R
import System.IO.Unsafe

quicksortR:: Array U DIM1 Double -> Array U DIM1 Double 
quicksortR xs = unsafePerformIO $ computeP $ qsortVect' (delay xs)

qsortVect':: Array D DIM1 Double -> Array D DIM1 Double
qsortVect' xs | (size (extent xs)) <=  1 = xs
              | otherwise =
  let p  = linearIndex xs ((size (extent xs)) `div` 2)
      ls = unsafePerformIO $ R.selectP (\idx -> (linearIndex xs idx) < p) (\idx -> linearIndex xs idx) (size (extent xs))--[x | x <- xs, x < p]
      gs = unsafePerformIO $ R.selectP (\idx -> (linearIndex xs idx) >= p) (\idx -> linearIndex xs idx) (size (extent xs))--[x | x <- xs, x >= p]

 in
 qsortVect' (delay ls) R.++ qsortVect' (delay gs)
