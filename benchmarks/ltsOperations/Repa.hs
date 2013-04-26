{-# LANGUAGE ScopedTypeVariables #-}

module Repa (segregateR, incrementAllR, lessThanR, zipR, zipsumR, sumR) where

import System.IO.Unsafe
import Data.Array.Repa as R
import Data.Array.Repa.Repr.LazyTreeSplitting as R
import Control.DeepSeq

zipR :: forall a b .Array L DIM1 a -> Array L DIM1 b -> Array L DIM1 (a,b) 
zipR as bs = R.zipWithLTS (\a -> \b -> (a,b)) as bs

zipsumR :: Num a => Array L DIM1 a -> Array L DIM1 a -> Array L DIM1 a
zipsumR xs ys = R.zipWithLTS (+) xs ys

sumR :: (NFData a, Num a) => Array L DIM1 a -> a
sumR xs = R.reduceLTS (+) 0 xs

lessThanR :: Ord a => Array L DIM1 a -> a -> Array L DIM1 a
lessThanR xs k = R.filterLTS (< k) xs

incrementAllR :: Array L DIM1 Int -> Array L DIM1 Int
incrementAllR xs = R.mapLTS (+1) xs

segregateR :: Array L DIM1 Int -> Int -> Array L DIM1 (Array L DIM1 Int)
segregateR xs k = R.mapLTS (\f -> R.filterLTS f xs) (R.ropeFromList (R.ix1 3) [(< k), (== k), (> k)])

