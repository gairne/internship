module IncsumRepa (incsumR) where

import System.IO.Unsafe
import Data.Array.Repa as R

--incsumR :: Array U DIM1 Int -> Array D DIM1 Int
--incsumR xs = (R.map (+1) xs)

incR :: Array U DIM1 Int -> Array D DIM1 Int
incR xs = R.map (+1) xs

sumR :: Array D DIM1 Int -> IO Int
sumR xs = R.sumAllP xs

incsumR :: Array U DIM1 Int -> Int
incsumR = unsafePerformIO . sumR . incR
