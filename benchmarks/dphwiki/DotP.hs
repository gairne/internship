{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
 
module DotP (dotp_wrapper)
where
 
--import qualified Prelude as P
import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double as PD
 
dotp_double :: [:Double:] -> [:Double:] -> Double
dotp_double xs ys = sumP [:x PD.* y | x <- xs | y <- ys:]
 
dotp_wrapper :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp_wrapper #-}
dotp_wrapper v w = dotp_double (fromPArrayP v) (fromPArrayP w)

