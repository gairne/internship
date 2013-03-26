import Timing
import Randomish
import System.Environment

import Data.Vector.Unboxed as VU

import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import Data.Array.Parallel.PArray.Scalar as PS

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as RU

import DPH

main :: IO ()
main
  = do args <- getArgs
       case args of
         [alg, size] -> run alg (read size)
         _          -> putStr $ unlines
                         [ "usage: $0 <alg> <size>"
                         , "  alg one of " Prelude.++ show ["dph"] ]

run alg size
  = do (result, tme) <- runAlg alg size
       putStr $ prettyTime tme

runAlg "dph" size
  = time $ let result = nqPA size in result `seq` return result