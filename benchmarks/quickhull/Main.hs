import DPH
import Point
import Criterion.Main
import Randomish
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

nPoints :: Int
nPoints = 10000

test :: [Point]
test = [(1.0, 0.0), (0.0, 3.0), (3.0, 2.0), (4.0,3.0), (7.0, 4.0), (8.0, 2.0), (10.0, 4.0), (8.0,5.0)]

main =
    let points = VU.zip (randomishDoubles nPoints xMin xMax seed1) (randomishDoubles nPoints yMin yMax seed2)
    in  putStrLn (show (quickHullPA (PS.fromUArray points)))
    --let points = VU.zip (randomishDoubles 1000 0.0 100.0 24156) (randomishDoubles 1000 0.0 100.0 61252)
    --in  putStrLn (show (quickHullPA (PA.fromList points)))
