{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module DPH (quickHullPA) where

import Point
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I

distance :: Point -> Line -> Double
distance (xo, yo) ((x1, y1), (x2, y2)) = (x1 D.- xo) D.* (y2 D.- yo) D.- (y1 D.- yo) D.* (x2 D.- xo)

hsplit :: [:Point:] -> Line -> [:Point:]
hsplit points line@(p1, p2)
  | lengthP packed I.== 0 = [:p1:]
  | otherwise
  = concatP [: hsplit packed ends | ends <- [:(p1, pm), (pm, p2):] :] --Hsplit with 2 new lines (ends)
  where
    cross  = [: distance p line | p <- points :] --[Distances]
    packed = [: p | (p,c) <- zipP points cross, c D.> 0.0 :] --[(Point, distance)] if over line in right direction
    pm     = points !: maxIndexP cross -- Max point


quickHull :: [:Point:] -> [:Point:]
quickHull points
  | lengthP points I.== 0 = points
  | otherwise
  = concatP [: hsplit points ends | ends <- [: (minx, maxx), (maxx, minx) :] :]
  where
    xs   = [: x | (x, y) <- points :]
    minx = points !: minIndexP xs
    maxx = points !: maxIndexP xs


quickHullPA :: PArray Point -> PArray Point
quickHullPA ps = toPArrayP (quickHull (fromPArrayP ps))
{-# NOINLINE quickHullPA #-}


