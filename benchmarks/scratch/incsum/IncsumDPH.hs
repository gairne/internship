{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module IncsumDPH (incsumDPH) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int as PI

incsum :: [:Int:] -> Int
incsum xs = sumP(mapP (PI.+ 1) xs)

incsumDPH :: PArray Int -> Int
{-# NOINLINE incsumDPH #-}
incsumDPH xs = incsum (fromPArrayP xs)

