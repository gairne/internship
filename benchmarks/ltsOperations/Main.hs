import System.Environment

import Data.Vector as VU

import Data.Array.Repa as R
import Data.Array.Repa.Repr.LazyTreeSplitting as RL

import Repa

import Criterion.Main
import Control.Exception as E

dSIZE :: Int
dSIZE = 1000000

main = do let vec1 = VU.toList (VU.enumFromN 0 dSIZE)
              vec2 = VU.toList (VU.enumFromN 0 dSIZE)
              rep1 = RL.ropeFromList (R.ix1 dSIZE) vec1
              rep2 = RL.ropeFromList (R.ix1 dSIZE) vec2
          print rep1
          print rep2
          defaultMain [
              bgroup "LTS"
               [ bench "segregate " $ whnf (segregateR rep1) (dSIZE `div` 3),
                 bench "increment " $ whnf incrementAllR rep1,
                 bench "lessThan  " $ whnf (lessThanR rep1) (dSIZE `div` 3),
                 bench "zip       " $ whnf (zipR rep1) rep2,
                 bench "zipsum    " $ whnf (zipsumR rep1) rep2,
                 bench "sum       " $ whnf sumR rep1
               ]
            ]
