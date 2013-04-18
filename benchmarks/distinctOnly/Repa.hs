module Repa ( distinctR ) where

import Data.Array.Repa as R
import System.IO.Unsafe

distinctR :: Array U DIM1 Int -> Array U DIM1 Int
distinctR xs = unsafePerformIO $ R.selectP (\i -> (unsafePerformIO $ R.foldAllP (\a -> \v -> if ((linearIndex xs i) == v) then a + 1 else a) 0 xs) == 1) (\i -> linearIndex xs i) (size (extent xs))
