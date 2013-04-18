module Repa (quickHullR) where

import System.IO.Unsafe
import Point
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Delayed as RD

distance :: Point -> Line -> Double
distance (xo, yo) ((x1, y1), (x2, y2)) = (x1 - xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

hsplit :: Array U DIM1 Point -> Line -> Array D DIM1 Point
hsplit points line@(p1, p2)
  | (size (extent packed)) == 0 = RD.fromFunction (R.ix1 1) (\_ -> p1)
  | otherwise = (hsplit packed (p1, pm)) R.++ (hsplit packed (pm, p2)) --concat (R.map (\line' -> hsplit packed line') (RD.fromFunction (R.ix1 2) (\i -> if i == 0 then (p1, pm) else (pm, p2))))
  where
  cross = R.map (\pt -> distance pt line) points
  packed = unsafePerformIO $ R.selectP (\i -> (linearIndex cross i) > 0.0) (\i -> fst (linearIndex (R.zipWith (\a -> \b -> (a, b)) points cross) i)) (size (extent cross))
  pm = fst $ unsafePerformIO $ (R.foldAllP (\a@(ap, ad) -> (\v@(vp, vd) -> if (vd > ad) then v else a)) (linearIndex points 0, linearIndex cross 0) (R.zipWith (\a -> \b -> (a,b)) points cross))

quickHullR :: Array U DIM1 Point -> Array U DIM1 Point
quickHullR points
  | (size (extent points)) == 0 = points
  | otherwise = unsafePerformIO $ computeP $ (hsplit points (minx, maxx)) R.++ (hsplit points (maxx, minx))
  where
    maxx = unsafePerformIO $ R.foldAllP (\a@(ax, ay) -> \v@(vx, vy) -> if vx > ax then v else a) (linearIndex points 0) points
    minx = unsafePerformIO $ R.foldAllP (\a@(ax, ay) -> \v@(vx, vy) -> if vx < ax then v else a) (linearIndex points 0) points
