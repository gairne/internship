import Timing
import System.Environment
import Randomish
import Point

import Data.Vector.Unboxed as VU

import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA hiding (nf)
import Data.Array.Parallel.PArray.Scalar as PS

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as RU

import DPH
import Repa

import Criterion.Main
import Control.Exception as E

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

dSIZE :: Int
dSIZE = 2000000

--main :: IO ()
--main = runCrit 10000
--  = do args <- getArgs
--       case args of
--         [alg, len] -> runCrit (read len)
--         _   -> putStr $ unlines
 --                        [ "usage: $0 <alg> <length>"
 --                        , "  alg one of " Prelude.++ show ["dph", "repa", "criterion"] ]

-- run alg len
--   = let vec1 = randomishDoubles len xMin xMax seed1
--         vec2 = randomishDoubles len yMin yMax seed2
--     in  if alg == "criterion" then runCrit vec1 vec2
--         else do
--           (result, tme) <- runAlg alg vec1 vec2
--           putStr $ prettyTime tme

-- runAlg "dph" vec1 vec2
--   = do let dph1 = PS.fromUArray2 (VU.zip vec1 vec2)
--        dph1 `seq` return ()
--        time $ let result = quickHullPA dph1 in result `seq` return ()

-- runAlg "repa" vec1 vec2
--   = do let rep1 = RU.fromUnboxed (R.ix1 (VU.length vec1)) (VU.zip vec1 vec2)
--        rep1 `seq` return ()
--        time $ let result = quickHullR rep1 in (size (extent result)) `seq` return ()

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--runCrit :: Vector Double -> Vector Double -> IO ()
main = do let vec1 = randomishDoubles dSIZE xMin xMax seed1
              vec2 = randomishDoubles dSIZE yMin yMax seed2
              rep1 = RU.fromUnboxed (R.ix1 (VU.length vec1)) (VU.zip vec1 vec2)
              dph1 = PS.fromUArray2 (VU.zip vec1 vec2)
          E.evaluate rep1
          E.evaluate dph1
          defaultMain [
              bgroup "QuickHull"
               [ --bench "DPH " $ whnf quickHullPA dph1,
                 bench "Repa" $ whnf quickHullR rep1
               ]
            ]
