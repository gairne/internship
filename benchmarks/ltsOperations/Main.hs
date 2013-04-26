import System.Environment

import Data.Vector.Unboxed as VU

import Data.Array.Repa as R
import Data.Array.Repa.Repr.LazyTreeSplitting as RL

import qualified Data.Array.Parallel.PArray as PA
import qualified Data.Array.Parallel.PArray.Scalar as PS

import Repa
import Vector
import DPH

import Criterion.Main
import Control.Exception as E

dSIZE :: Int
dSIZE = 1000000

main = do let vec1 = (VU.enumFromN 0 dSIZE)
              vec2 = (VU.enumFromN 0 dSIZE)
              dph1 = PS.fromUArray vec1
              dph2 = PS.fromUArray vec2
              rep1 = RL.ropeFromList (R.ix1 dSIZE) (VU.toList vec1)
              rep2 = RL.ropeFromList (R.ix1 dSIZE) (VU.toList vec2)
          print rep1
          print rep2
          print dph1
          print dph2
          defaultMain [
              bgroup "LTS"
               [ bench "increment " $ whnf incrementAllR rep1,
                 bench "lessThan  " $ whnf (lessThanR rep1) (dSIZE `div` 3),
                 bench "zip       " $ whnf (zipR rep1) rep2,
                 bench "zipsum    " $ whnf (zipsumR rep1) rep2,
                 bench "sum       " $ whnf sumR rep1
               ]
              , bgroup "Vector"
               [ bench "increment " $ whnf incrementAllV vec1,
                 bench "lessThan  " $ whnf (lessThanV vec1) (dSIZE `div` 3),
                 bench "zip       " $ whnf (zipV vec1) vec2,
                 bench "zipsum    " $ whnf (zipsumV vec1) vec2,
                 bench "sum       " $ whnf sumV vec1
               ]
              , bgroup "DPH"
               [ bench "increment " $ whnf (incrementAllPA) dph1,
                 bench "lessThan  " $ whnf (lessThanPA dph1) (dSIZE `div` 3),
                 bench "zip       " $ whnf (zipPA dph1) dph2,
                 bench "zipsum    " $ whnf (zipsumPA dph1) dph2,
                 bench "sum       " $ whnf sumPA dph1
               ]
            ]

