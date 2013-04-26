{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module DPH (incrementAllPA, lessThanPA, zipPA, zipsumPA, sumPA) where

import System.IO.Unsafe
import Data.Array.Parallel as PA
import Data.Array.Parallel.Prelude.Int as I
import Data.Array.Parallel.PArray.Scalar as PS
import Control.DeepSeq

zipPA :: PArray Int -> PArray Int -> PArray (Int, Int) 
zipPA as bs = toPArrayP (zip' (fromPArrayP as) (fromPArrayP bs))
zip' :: [: Int :] -> [: Int :] -> [: (Int, Int) :]
zip' as bs = PA.zipP as bs

zipsumPA :: PArray Int -> PArray Int -> PArray Int
zipsumPA xs ys = toPArrayP (zipsum' (fromPArrayP xs) (fromPArrayP ys))
zipsum' :: [: Int :] -> [: Int :] -> [: Int :]
zipsum' xs ys = PA.zipWithP (I.+) xs ys

sumPA :: PArray Int -> Int
sumPA xs = 0
--sum' :: PArray Int -> Int
--sum' xs = PS.fold (I.+) 0 xs

lessThanPA :: PArray Int -> Int -> PArray Int
lessThanPA xs k = toPArrayP (lessThan' (fromPArrayP xs) k)
lessThan' :: [: Int :] -> Int -> [: Int :]
lessThan' xs k = PA.filterP (I.< k) xs

incrementAllPA :: PArray Int -> PArray Int
incrementAllPA xs = toPArrayP (incrementAll' (fromPArrayP xs))
incrementAll' :: [: Int :] -> [: Int :]
incrementAll' xs = PA.mapP (I.+ 1) xs

--segregatePA :: PArray Int -> Int -> PArray (PArray Int)
--segregatePA xs k = toPArrayP (segregate' (fromPArrayP xs) k)
--segregate' :: [: Int :] -> Int -> [: [: Int :] :]
--segregate' xs k = [: [: x | x <- xs, op x :] | op <- [: (<k), (==k), (>k) :] :]
