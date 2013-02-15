module IncsumRepa (incsumR) where

import System.IO.Unsafe
import Data.Array.Repa as R

--incsumR :: Array U DIM1 Int -> Array D DIM1 Int
--incsumR xs = (R.map (+1) xs)

incR :: Array U DIM1 Int -> Array U DIM1 Int
incR xs = unsafePerformIO $ computeP $ (R.map (+1) xs)

sumR :: Array U DIM1 Int -> Int
sumR xs = sum (R.toList xs)

incsumR' :: Array U DIM1 Int -> Int
incsumR' xs = sumR (incR xs)

incsumR :: Array U DIM1 Int -> Int
incsumR xs = sum (R.toList (unsafePerformIO $ computeP $ (R.map (+1) xs)::(Array U DIM1 Int)))
