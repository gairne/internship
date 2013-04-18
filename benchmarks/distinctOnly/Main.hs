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
import Repa

--import qualified Control.Exception as E

sEED1 :: Int
sEED1 = 12345

main :: IO ()
main
  = do args <- getArgs
       case args of
         [alg, len, range] -> run alg (read len) (read range)
         _          -> putStr $ unlines
                         [ "usage: $0 <alg> <length> <range>"
                         , "  alg one of " Prelude.++ show ["dph", "repa"] ]

run alg len range
  = do let vec1 :: VU.Vector Int
           vec1 = randomishInts len 0 range sEED1
           
       (result, tme) <- runAlg alg vec1
       putStr $ prettyTime tme

runAlg "dph" vec1
  = do let dph1 = PS.fromUArray vec1
       dph1 `seq` return ()
       time $ let result = distinctPA dph1 in result `seq` return ()

runAlg "repa" vec1
  = do let rep1 = RU.fromUnboxed (R.ix1 (VU.length vec1)) vec1
       rep1 `seq` return ()
       time $ let result = distinctR rep1 in result `seq` return ()
