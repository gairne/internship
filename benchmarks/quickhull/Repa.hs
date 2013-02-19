module Repa (quickHullR) where

import System.IO.Unsafe
import Point
import Data.Array.Repa as R

--(1) Find minX, maxX (+1 traverse non parallel). Call (2) with Line(minX Point, maxX Point) twice with minX and maxX transposed for second call
--(2) map (distance (minXP, maxXP) and > 0) points (+1 traverse)
--(3) calculate minX maxX (remember this point for return later), 2 new lines, p to minX, p to maxX. Call (2) for each line.

lineSplit :: Array D DIM1 Point -> Line -> Array D DIM1 Point
lineSplit points (p1, p2)
  | size (extent allAboveLine) == 0 = delay (R.fromListUnboxed (R.ix1 1) [maxPt])
  | otherwise = delay (R.fromListUnboxed (R.ix1 1) [maxPt]) R.++ leftLineRecurse R.++ rightLineRecurse
    where
    leftLineRecurse = lineSplit extractAbove (p1, maxPt)
    rightLineRecurse = lineSplit extractAbove (maxPt, p2)
    allAboveLine = aboveLine points (p1, p2)
    maxPt = maxPoint allAboveLine
    --emptyListR :: Array D DIM1 Point
    --emptyListR = R.fromListUnboxed([])
    --constructListR e = R.fromListUnboxed([e])
    --extractAbove :: Array D DIM1 Point
    --allAboveLine :: -> Array D DIM1 (Point, Double)
    --R.foldAllP (snd (Point, Double) check > 0.0, if so include) emptyList Array(Point, Double)
    extractAbove = R.foldAllP (\new -> \list -> if snd new > 0.0 then (R.fromListUnboxed (R.ix1 1) [fst new]) R.++ list else list) R.fromListUnboxed([]) allAboveLine
    --extractAbove = foldr (\new -> \list -> if (snd new) > 0.0 then (f

maxPoint :: Array D DIM1 (Point, Double) -> Point
maxPoint xs = fst (unsafePerformIO $ maxPointDouble xs)
  where
  maxPointDouble :: Array D DIM1 (Point, Double) -> IO (Point, Double)
  maxPointDouble xs = R.foldAllP (\new -> \cur -> if (snd new) > (snd cur) then new else cur) ((0.0, 0.0), 0.0) xs

--aboveLine => maxPoint (remember) => recurse for two new lines

-- Return an array populated with points above a horizontal line. Points below the line have distance of zero.
aboveLine :: Array D DIM1 Point -> Line -> Array D DIM1 (Point, Double)
aboveLine ps (p1, p2) = R.zipWith (\a -> \b -> (a, b)) ps distances
  where distances = R.map (\p -> if (distance p (p1, p2)) > 0.0 then distance p (p1, p2) else 0.0) ps



--belowLine :: Array D DIM1 Double -> Line -> Array D DIM1 Double
--belowLine ps (p1, p2) = R.zip ps (R.map (\p -> if (distance p (p1, p2)) < 0.0 then p else 0.0) ps)

distance :: Point -> Line -> Double
distance (xo, yo) ((x1, y1), (x2, y2)) = (x1 - xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

quickHullR :: Array U DIM1 Point -> Array U DIM1 Point
quickHullR points = points -- lineSplit points minX maxX
--  where
--  minX = 
--  maxX = 
