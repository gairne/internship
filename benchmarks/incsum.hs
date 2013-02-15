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
       let p10m = PA.fromList [1..10000000]
       let s10m = U.fromList [1..10000000]
       let r10m = (RU.fromListUnboxed (Z :. (10000000::Int)) [1..10000000])
       --let x = r10m `deepSeqArray` s10m `deepseq` 0 -- Has no effect on mean
       let a = incsum s10m
       let b = incsumDPH p10m --This seems to benefit completetion times of DPH ~~800ms for 10m to ~~200ms
       let c = incsumR r10m --This doesn't seem to have an affect on Repa
       putStrLn(show(a +  b + c))
       defaultMainWith cfg (return ()) [
           bgroup "incsum"
             [ bench "incsumSeq 10mil" $ nf incsum s10m,
               bench "incsumPar 10mil" $ nf incsumDPH p10m,
               bench "incsumRep 10mil" $ nf incsumR r10m
               --,bench "incsumRep 10mil" $ nf incsumR r10m,
               --bench "incsumPar 10mil" $ nf incsumDPH p10m,
               --bench "incsumSeq 10mil" $ nf incsum s10m
             ]
         ]
--let result = incsumW (PA.fromList [1..10000000])
--       in print result
