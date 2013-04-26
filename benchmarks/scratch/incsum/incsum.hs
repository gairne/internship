{-# LANGUAGE PackageImports #-}

import Timing
import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import qualified Control.Exception as E
--import Criterion.Main
--import Criterion.Config
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RU

import Data.Array.Repa

import Control.DeepSeq

import IncsumDPH
import IncsumRepa

sIZE :: Int
sIZE = 1000000

incsum :: U.Vector Int -> Int
incsum xs = U.sum $ U.map (+1) xs

main = do
  let v :: U.Vector Int
      v = U.generate sIZE id
      pa :: PA.PArray Int
      pa = PA.fromList [0..sIZE]
      ra :: R.Array U DIM1 Int
      ra = RU.fromUnboxed (R.ix1 sIZE) v
  -- Force to whnf
  E.evaluate v
  E.evaluate pa
  E.evaluate ra
  (x, tme) <- time $ let result = incsumR ra in result `seq` return result
  putStr $ prettyTime tme
--  defaultMainWith defaultConfig (return ()) [
--      bgroup "incsum"
--        [ bench "incsumSeq 1mil" $ nf incsum v,
--          bench "incsumPar 1mil" $ nf incsumDPH pa,
--          bench "incsumRep 1mil" $ nf incsumR ra
--        ]
--    ]
