import NqueensDPH
import NqueensList
import Criterion.Main
import Criterion.Config
import Data.Array.Parallel

main = defaultMainWith defaultConfig (return ()) [
         bgroup "nq"
           [ bench "nqDPH 1mil" $ whnf nqPA 12,
             bench "nq 1mil" $ whnf nqList 12
           ]
       ]
