{-# LANGUAGE ScopedTypeVariables #-}

module Vector (incrementAllV, lessThanV, zipV, zipsumV, sumV) where

import System.IO.Unsafe
import Data.Vector as V
import Data.Vector.Unboxed as VU
import Control.DeepSeq

zipV :: (Unbox a, Unbox b) => VU.Vector a -> VU.Vector b -> VU.Vector (a,b) 
zipV as bs = VU.zip as bs

zipsumV :: (Unbox a, Num a) => VU.Vector a -> VU.Vector a -> VU.Vector a
zipsumV xs ys = VU.zipWith (+) xs ys

sumV :: (NFData a, Num a, Unbox a) => VU.Vector a -> a
sumV xs = VU.foldl' (+) 0 xs

lessThanV :: (Ord a, Unbox a) => VU.Vector a -> a -> VU.Vector a
lessThanV xs k = VU.filter (< k) xs

incrementAllV :: VU.Vector Int -> VU.Vector Int
incrementAllV xs = VU.map (+1) xs

--segregate :: Vector Int -> Int -> Vector (Vector Int)
--segregate xs k = R.mapLTS (\f -> R.filterLTS f xs) (R.ropeFromList (R.ix1 3) [(< k), (== k), (> k)])

