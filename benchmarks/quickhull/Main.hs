import Timing
import System.Environment
import Randomish

import Point
import DPH

import Data.Vector.Unboxed as VU
import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import qualified Data.Array.Parallel.PArray.Scalar as PS

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
         [n] -> run (read n)
         _   -> putStr $ "usage: $0 <size>"



run nPoints
  = do let points = PS.fromUArray2 (VU.zip (randomishDoubles nPoints xMin xMax seed1) (randomishDoubles nPoints yMin yMax seed2))
       points `seq` return ()
       (result, tme) <- time $ let result = quickHullPA points in result `seq` return result      

       putStr $ prettyTime tme

