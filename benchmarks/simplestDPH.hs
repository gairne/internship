{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module SimplestDPH (incsumW) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int as PI

incsum :: [:Int:] -> Int
incsum xs = sumP(mapP (PI.+ 1) xs)

incsumW :: PArray Int -> Int
{-# NOINLINE incsumW #-}
incsumW xs = incsum (fromPArrayP xs)

