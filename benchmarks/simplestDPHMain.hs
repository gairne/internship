import Data.Array.Parallel
import Data.Array.Parallel.PArray as PA

import SimplestDPH

main = let result = incsumW (PA.fromList [1,2,3,4,5,6,7,8,9,10])
       in print result
