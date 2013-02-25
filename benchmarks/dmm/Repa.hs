module Repa ( dotR ) where

import Data.Array.Repa as R
import System.IO.Unsafe

dotR :: Array U DIM1 Double -> Array U DIM1 Double -> Double
dotR v w = dotp' v w

dotp' :: Array U DIM1 Double -> Array U DIM1 Double -> Double
dotp' v w = unsafePerformIO $ R.sumAllP (R.zipWith (*) v w)
