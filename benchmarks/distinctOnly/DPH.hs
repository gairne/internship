{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module DPH ( distinctPA ) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int as I

import qualified Prelude

distinctPA :: PArray Int -> PArray Int
distinctPA xs = (toPArrayP (distinctPA' (fromPArrayP xs)))
{-# NOINLINE distinctPA #-}

distinctPA' :: [: Int :] -> [: Int :]
distinctPA' xs = [: x | x <- xs, (lengthP [: y | y <- xs, x I.== y :]) I.== 1 :]