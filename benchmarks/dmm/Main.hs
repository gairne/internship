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
import Vector

import qualified Control.Exception as E

sEED1 :: Int
sEED1 = 12345

sEED2 :: Int
sEED2 = 54329

main :: IO ()
main
  = do args <- getArgs
       case args of
         [alg, len] -> run alg (read len)
         _          -> putStr $ unlines
                         [ "usage: $0 <alg> <length>"
                         , "  alg one of " Prelude.++ show ["dph", "repa"] ]

run alg len
  = do let vec1 :: VU.Vector Double
           vec1 = randomishDoubles len 0 1 sEED1
           vec2 :: VU.Vector Double
           vec2 = randomishDoubles len 0 1 sEED2
           
       (result, tme) <- runAlg alg vec1 vec2
       putStr $ prettyTime tme

runAlg "dph" vec1 vec2
  = do let dph1 = PS.fromUArray vec1
           dph2 = PS.fromUArray vec2
       dph1 `seq` dph2 `seq` return ()
       E.evaluate dph1
       E.evaluate dph2
       time $ let result = dotPA dph1 dph2 in result `seq` return result -- Evaluation wise, OK. Returns a Double

runAlg "repa" vec1 vec2
  = do let rep1 = RU.fromUnboxed (R.ix1 (VU.length vec1)) vec1
           rep2 = RU.fromUnboxed (R.ix1 (VU.length vec2)) vec2
       rep1 `seq` rep2 `seq` return ()
       E.evaluate rep1
       E.evaluate rep2
       time $ let result = dotR rep1 rep2 in result `seq` return result -- Evaluation wise, OK. Returns a Double
