import Timing
import Randomish
import System.Environment

import Data.Vector.Unboxed as VU

import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import Data.Array.Parallel.PArray.Scalar as PS

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as RU

import Vectorised
import Repa

--import qualified Control.Exception as E

sEED1 :: Int
sEED1 = 12345

main :: IO ()
main
  = do args <- getArgs
       case args of
         [alg, n] -> run alg (read n)
         _   -> putStr $ "usage: $0 <alg> <size>"

run alg nPoints
  = do let vec1 :: VU.Vector Double
           vec1 = randomishDoubles nPoints 0 1 sEED1

       (result, tme) <- runAlg alg vec1
       putStr $ prettyTime tme

runAlg "dph" vec1
  = do let dph1 = PS.fromUArray vec1
       dph1 `seq` return ()
       time $ let result = quicksortPA dph1 in result `seq` return ()

runAlg "repa" vec1
  = do let rep1 = RU.fromUnboxed (R.ix1 (VU.length vec1)) vec1
       rep1 `seq` return ()
       time $ let result = quicksortR rep1 in result `seq` return ()
