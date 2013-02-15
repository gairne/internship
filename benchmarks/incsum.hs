import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import Criterion.Main
import Criterion.Config
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Repa.Repr.Unboxed as RU

import Data.Array.Repa

import Control.DeepSeq

import IncsumDPH
import IncsumRepa

cfg = defaultConfig { cfgReport = ljust "/home/t-mmole/incsum.html" }

incsum :: U.Vector Int -> Int
incsum xs = U.sum $ U.map (+1) xs

main = do
       let p1m = PA.fromList [1..1000000]
       let s1m = U.fromList [1..1000000]
       let r1m = (RU.fromListUnboxed (Z :. (1000000::Int)) [1..1000000])
       --let x = r10m `deepSeqArray` s10m `deepseq` 0 -- Has no effect on mean
       let a = incsum s1m
       let b = incsumDPH p1m --This seems to benefit completetion times of DPH ~~800ms for 10m to ~~200ms
       let c = incsumR r1m --This doesn't seem to have an affect on Repa
       putStrLn(show(a +  b + c))
       defaultMainWith cfg (return ()) [
           bgroup "incsum"
             [ bench "incsumSeq 1mil" $ nf incsum s1m,
               bench "incsumPar 1mil" $ nf incsumDPH p1m,
               bench "incsumRep 1mil" $ nf incsumR r1m
             ]
         ]
