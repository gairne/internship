import Randomish
import Vectorised as Z
import Data.Vector.Unboxed		(Vector)
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P hiding (nf)
import qualified Data.Vector.Unboxed	as V
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as RU
import Repa as M
import qualified Data.Array.Parallel.PArray.Scalar as PS

import Criterion.Main
import Criterion.Config
import qualified Control.Exception as E

sIZE :: Int
sIZE = 100000

sEED1 :: Int
sEED1 = 12345

main = do
  let vec1 :: V.Vector Double
      vec1 = randomishDoubles sIZE 0 1 sEED1
      dph1 :: P.PArray Double
      dph1 = PS.fromUArray vec1
      rep1 :: R.Array R.U R.DIM1 Double
      rep1 = RU.fromListUnboxed (R.ix1 (V.length vec1)) (V.toList vec1) --THIS IS LIKELY TO BE SLOW
  -- Force to whnf
  E.evaluate vec1
  E.evaluate dph1
  E.evaluate rep1
  defaultMainWith defaultConfig (return ()) [
      bgroup "QuickSort"
        [ bench "qsDPH" $ nf P.length(Z.quicksortPA dph1),
          bench "qsRepa" $ nf size(extent (M.quicksortR rep1))
        ]
    ]
