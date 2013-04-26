{-# LANGUAGE BangPatterns, PatternGuards #-}

-- | The list version of the solver also builds the bounding box at every
--   node of the tree, which is good for visualisation.
module Solver.Repa.Solver
	( MassPoint	(..)
	, BoundingBox	(..)
	, BHTree	(..)
	, calcAccels
	, buildTree
	, findBounds)
where
import Common.Body
import Data.Array.Repa as R
import Data.Array.Repa.Repr.LazyTreeSplitting as R
import Control.DeepSeq
	
type BoundingBox
	= (Double, Double, Double, Double)
	
sizeOfBox :: BoundingBox -> Double
{-# INLINE sizeOfBox #-}
sizeOfBox (llx, lly, rux, ruy)
	= min (abs (rux - llx)) (abs (ruy - lly))


-- | The Barnes-Hut tree we use to organise the points.
data BHTree
	= BHT
	{ bhTreeSize	:: {-# UNPACK #-} !Double	-- minimum of hight and width of cell
	, bhTreeCenterX	:: {-# UNPACK #-} !Double
	, bhTreeCenterY	:: {-# UNPACK #-} !Double
	, bhTreeMass	:: {-# UNPACK #-} !Double
	, bhTreeBranch	:: Array L DIM1 BHTree }
	deriving Show


-- | Compute the acclerations on all these points.
calcAccels :: Double -> Array L DIM1 MassPoint -> Array L DIM1 Accel
calcAccels epsilon mpts
 = R.mapLTS (calcAccel epsilon (buildTree mpts)) mpts


-- | Build a Barnes-Hut tree from these points.
buildTree :: Array L DIM1 MassPoint -> BHTree
buildTree mpts
	= buildTreeWithBox (findBounds mpts) mpts

type D4 = (Double, Double, Double, Double)
type MassPoint' = D4

-- | Find the coordinates of the bounding box that contains these points.
findBounds :: Array L DIM1 MassPoint -> (Double, Double, Double, Double)
{-# INLINE findBounds #-}
findBounds bounds
 = R.reduceLTS acc (x1, y1, x1, y1) (R.mapLTS (\(a,b,c) -> (a,b,c,0.0)) bounds)
 where
	(x1, y1, _)	= R.linearIndex bounds 0

        acc :: (D4 -> MassPoint' -> D4) -- -> D4 -> MassPoint' -> D4
	acc (!llx, !lly, !rux, !ruy) (x, y, _, _)
	 = let	!llx'	= min llx  x
		!lly'	= min lly  y
		!rux'	= max rux  x
		!ruy'	= max ruy  y
	   in	(llx', lly', rux', ruy')


-- | Given a bounding box that contains all the points, 
--   build the Barnes-Hut tree for them.
buildTreeWithBox
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> Array L DIM1 MassPoint	-- ^ points in the box.
	-> BHTree

buildTreeWithBox bb mpts
  | (size (extent mpts)) <= 1	= BHT s x y m (R.ropeFromList (R.ix1 0) [])
  | otherwise			= BHT s x y m subTrees
  where	
	s			= sizeOfBox bb
	(x, y, m)		= calcCentroid   mpts
    	(boxes, splitPnts)	= splitPoints bb mpts
    	subTrees		= R.zipWithLTS (\bb' -> \ps -> buildTreeWithBox bb' ps) boxes splitPnts
                            -- [buildTreeWithBox bb' ps | (bb', ps) <- zip boxes splitPnts]

  
-- | Split massPoints according to their locations in the quadrants.
splitPoints
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> Array L DIM1 MassPoint	-- ^ points in the box.
	-> ( Array L DIM1 BoundingBox	-- 
	   , Array L DIM1 (Array L DIM1 MassPoint))

splitPoints b@(llx, lly, rux, ruy) mpts
  | noOfPoints <= 1 = (R.ropeFromList (R.ix1 1) [b], R.ropeFromList (R.ix1 1) [mpts])
  | otherwise         
  = let x = R.filterLTS (\(b,p) -> size (extent p) > 0) (R.zipWithLTS (\b -> \p -> (b,p)) boxes splitPars)
    in (R.mapLTS fst x, R.mapLTS snd x)
--unzip [ (b,p) | (b,p) <- zip boxes splitPars, V.length p > 0]
  where
        noOfPoints	= size (extent mpts)

	-- The midpoint of the parent bounding box.
        (midx,  midy)	= ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 

	-- Split the parent bounding box into four quadrants.
        b1		= (llx,  lly,  midx,  midy)
        b2		= (llx,  midy, midx,  ruy)
        b3		= (midx, midy, rux,   ruy)
        b4		= (midx, lly,  rux,   midy)
        boxes		= R.ropeFromList (R.ix1 4) [b1, b2, b3, b4]

	-- Sort the particles into the smaller boxes.
        lls		= R.filterLTS (inBox b1) mpts
        lus		= R.filterLTS (inBox b2) mpts
        rus		= R.filterLTS (inBox b3) mpts
        rls		= R.filterLTS (inBox b4) mpts
        splitPars	= R.ropeFromList (R.ix1 4) [lls, lus, rus, rls]


-- | Check if a particle is in box (excluding left and lower border)
inBox:: BoundingBox -> MassPoint -> Bool
{-# INLINE inBox #-}
inBox (llx, lly, rux, ruy) (px, py, _) 
	= (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)

rSum :: (NFData a, Num a) => Array L DIM1 a -> a
rSum xs = R.reduceLTS (+) 0 xs

-- | Calculate the centroid of some points.
calcCentroid :: Array L DIM1 MassPoint -> MassPoint
{-# INLINE calcCentroid #-}
calcCentroid mpts 
  = (rSum xs / mass, rSum ys / mass, mass)
  where	
   mass     = rSum   $ R.mapLTS (\(_, _, m) -> m) mpts
   (xs, ys) = f
   f = let i = R.mapLTS (\(x, y, m) -> (m * x, m * y)) mpts
       in (R.mapLTS fst i, R.mapLTS snd i)


-- | Calculate the accelleration of a point due to the points in the given tree.
calcAccel:: Double -> BHTree -> MassPoint -> (Double, Double)
calcAccel !epsilon (BHT s x y m subtrees) mpt
	| (size (extent subtrees)) == 0 --[]	<- subtrees
	= accel epsilon mpt (x, y, m)
	
	| isFar mpt s x y
	= accel epsilon mpt (x, y, m)

	| otherwise
	= R.reduceLTS (\(a1, a2) -> \(v1, v2) -> ((a1+v1), (a2+v2))) (0, 0) (R.mapLTS (\st -> calcAccel epsilon st mpt) subtrees)
--let	(xs, ys)  = unzip [ calcAccel epsilon st mpt | st <- subtrees]
--in	(sum xs, sum ys) 


-- | If the point is far from a cell in the tree then we can use
--   it's centroid as an approximation of all the points in the region.
--
isFar 	:: MassPoint 	-- point being accelerated
	-> Double	-- size of region
	-> Double	-- position of center of mass of cell
	-> Double	-- position of center of mass of cell
	-> Bool

{-# INLINE isFar #-}
isFar (x1, y1, m) s x2 y2 
 = let	!dx	= x2 - x1
	!dy	= y2 - y1
	!dist	= sqrt (dx * dx + dy * dy)
   in	(s / dist) < 1


