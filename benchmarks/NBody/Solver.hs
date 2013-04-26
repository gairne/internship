
-- | Wrappers for the various solvers.
module Solver
	( Solver
	, solvers)
where
import Common.Body

import qualified Solver.ListBH.Solver		as SolverLB

import qualified Data.Vector.Unboxed		as V
import qualified Solver.VectorBH.Solver		as SolverVB
import qualified Solver.VectorNaive.Solver	as SolverVN

import qualified Data.Array.Repa                as R
import qualified Data.Array.Repa.Repr.LazyTreeSplitting as R
import qualified Data.Rope.Rope                 as RP
import qualified Solver.Repa.Solver             as SolverR

import qualified Data.Array.Parallel	as P
import qualified Data.Array.Parallel.PArray	as P
import qualified Solver.NestedBH.Solver		as SolverNB

import Debug.Trace

import System.IO.Unsafe

type Solver	= Double -> V.Vector MassPoint -> V.Vector Accel

solvers :: [(String, Solver)]
solvers
 = 	[ ("list-bh",		calcAccels_lb)
	, ("vector-naive",	calcAccels_vn)
	, ("vector-bh",		calcAccels_vb)
	, ("nested-bh",		calcAccels_nb)
        , ("repa",              calcAccels_r ) ]


-- | Lists + Barnes-Hut algorithm.
calcAccels_lb	:: Solver
calcAccels_lb epsilon mpts
	= V.fromList
	$ SolverLB.calcAccels epsilon
	$ V.toList mpts


-- | Vector + Naive algorithm.
calcAccels_vn	:: Solver
calcAccels_vn epsilon
	= SolverVN.calcAccels epsilon 
	

-- | Vector + Barnes-Hut algorithm.
calcAccels_vb 	:: Solver
calcAccels_vb epsilon mpts
	= SolverVB.calcAccels epsilon mpts


-- | Nested Data Parallelism + Barnes-Hut algorithm.
calcAccels_nb	:: Solver
calcAccels_nb epsilon mpts
 = let	
	-- bounds finding isn't vectorised yet.
	(llx, lly, rux, ruy)	= SolverVB.findBounds mpts

	mpts'	= P.fromList $ V.toList mpts
	accels'	= SolverNB.calcAccelsWithBoxPA epsilon llx lly rux ruy mpts'
	
   in	V.fromList $ P.toList accels'

calcAccels_r :: Solver
calcAccels_r epsilon mpts
 = let x = R.fromRope (R.ix1 (V.length mpts)) $ RP.fromVector mpts
   in unsafePerformIO (R.setGlobalLeafSize 512) `seq` RP.toVector $ R.toRope $ SolverR.calcAccels epsilon x  
