{-# LANGUAGE PackageImports #-}

import Timing
import Randomish
import System.Environment
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P hiding (nf, length)
import qualified Data.Vector.Unboxed	as V
import qualified Vector	                as V
import qualified Vectorised	        as Z
import qualified Repa                   as M
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RU
import qualified Data.Array.Parallel.Unlifted as PU
import qualified Data.Array.Parallel.PArray.Scalar as PS

--import Criterion.Main
--import Criterion.Config
import qualified Control.Exception as E

sEED1 :: Int
sEED1 = 12345
sEED2 :: Int
sEED2 = 54329

main :: IO ()
main
  = do args <- getArgs
       case args of
         [alg, len] -> run alg (read len)
         _          -> putStr $ unlines
                         [ "usage: $0 <alg> <length>"
                         , "  alg one of " ++ show ["dph", "repa"] ]

run alg len
  = do let vec1 :: V.Vector Double
           vec1 = randomishDoubles len 0 1 sEED1
           vec2 :: V.Vector Double
           vec2 = randomishDoubles len 0 1 sEED2
           
       (result, tme) <- runAlg alg vec1 vec2

       putStr $ prettyTime tme

runAlg "dph" vec1 vec2
  = do let dph1 = PS.fromUArray vec1
           dph2 = PS.fromUArray vec2
       dph1 `seq` dph2 `seq` return ()
       time $ let result = Z.dotPA dph1 dph2 in result `seq` return result

runAlg "repa" vec1 vec2
  = do let rep1 = RU.fromListUnboxed (R.ix1 (V.length vec1)) (V.toList vec1) --FIND ANOTHER FUNCTION!
           rep2 = RU.fromListUnboxed (R.ix1 (V.length vec2)) (V.toList vec2) --THIS IS SLOW!
       rep1 `seq` rep2 `seq` return ()
       time $ let result = M.dotR rep1 rep2 in result `seq` return result

--  -- Force to whnf
--  E.evaluate vec1
--  E.evaluate vec2
--  E.evaluate dph1
--  E.evaluate dph2
--  E.evaluate rep1
--  E.evaluate rep2
--  defaultMainWith defaultConfig (return ()) [
--      bgroup "DMM"
--        [ bench "dmmDPH" $ nf (Z.dotPA dph1) dph2,
--          bench "dmmRepa" $ nf (M.dotR rep1) rep2
--        ]
--    ]
