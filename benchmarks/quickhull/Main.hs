import Timing
import System.Environment
import Randomish

import Point
import DPH
import Repa

import Data.Vector.Unboxed as VU
import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import qualified Data.Array.Parallel.PArray.Scalar as PS

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as RU

xMin :: Double
xMin = 0.0

xMax :: Double
xMax = 1000.0

yMin :: Double
yMin = 0.0

yMax :: Double
yMax = 1000.0

seed1 :: Int
seed1 = 23541
seed2 :: Int
seed2 = 46145

main :: IO ()
main
  = do args <- getArgs
       case args of
         [alg, n] -> run alg (read n)
         _   -> putStr $ "usage: $0 <alg> <size>"

run alg nPoints
  = do let vec1 :: VU.Vector Double
           vec1 = randomishDoubles nPoints xMin xMax seed1
           vec2 :: VU.Vector Double
           vec2 = randomishDoubles nPoints yMin yMax seed2
       
       (result, tme) <- runAlg alg vec1 vec2 nPoints
       putStr $ prettyTime tme

runAlg "dph" vec1 vec2 n
  = do let dph1 = PS.fromUArray2 (VU.zip vec1 vec2)
       dph1 `seq` return ()
       time $ let result = quickHullPA dph1 in result `seq` return ()

runAlg "repa" vec1 vec2 n
  = do let rep1 = RU.fromUnboxed (R.ix1 n) (VU.zip vec1 vec2)
       rep1 `seq` return ()
       time $ let result = quickHullR rep1 in ((size (extent result))) `seq` return ()


