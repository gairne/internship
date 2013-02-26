{-# LANGUAGE PackageImports #-}

import Randomish
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P hiding (nf, length)
import qualified Data.Vector.Unboxed	as V
import qualified Vector	                as V
import qualified Vectorised	        as Z
import qualified Repa                   as M
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RU

import Criterion.Main
import Criterion.Config
import qualified Control.Exception as E

sIZE :: Int
sIZE = 1000000

sEED1 :: Int
sEED1 = 12345
sEED2 :: Int
sEED2 = 54329

main = do 
  let vec1 :: V.Vector Double
      vec1 = randomishDoubles sIZE 0 1 sEED1
      vec2 :: V.Vector Double
      vec2 = randomishDoubles sIZE 0 1 sEED2
      dph1 :: P.PArray Double
      dph1 = P.fromList (V.toList vec1)
      dph2 :: P.PArray Double
      dph2 = P.fromList (V.toList vec2)
      rep1 :: R.Array R.U R.DIM1 Double
      rep1 = RU.fromListUnboxed (R.ix1 (V.length vec1)) (V.toList vec1)
      rep2 :: R.Array R.U R.DIM1 Double
      rep2 = RU.fromListUnboxed (R.ix1 (V.length vec2)) (V.toList vec2)
  -- Force to whnf
  E.evaluate vec1
  E.evaluate vec2
  E.evaluate dph1
  E.evaluate dph2
  E.evaluate rep1
  E.evaluate rep2
  defaultMainWith defaultConfig (return ()) [
      bgroup "DMM"
        [ bench "dmmDPH" $ nf (Z.dotPA dph1) dph2,
          bench "dmmRepa" $ nf (M.dotR rep1) rep2
        ]
    ]
