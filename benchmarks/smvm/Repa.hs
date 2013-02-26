module Repa (smvmR) where

import Data.Array.Repa as R
import System.IO.Unsafe

-- Of course loses all benefit of sparseness!

smvmSum :: Array U DIM2 (Int, Double) -> Array U DIM1 Double -> Array U DIM1 Double
smvmSum m v = unsafePerformIO $ R.sumP (smvmR m v)

smvmR :: Array U DIM2 (Int, Double) -> Array U DIM1 Double -> Array D DIM2 Double
smvmR m v = R.map (\(i,x) -> x * (linearIndex v i)) m