{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module MSTDPH (kruskalPA) where

import Data.Array.Parallel as P
import Data.Array.Parallel.Prelude.Int as I
import Data.Array.Parallel.Prelude.Bool as B
import Data.Array.Parallel.Prelude.Tuple as T

type Node = Int
type Edge = (Node, Node)
type Graph = [:Edge:]

-- Find the sub MST that a node is in or [] if not
nodeInMSTs :: [:Graph:] -> Node -> Graph
nodeInMSTs msts n = concatP (mapP (\mst -> if (nodeInMST n mst) then mst else emptyP) msts)

nodeInMST :: Node -> Graph -> Bool
nodeInMST n es = orP (mapP (\e@(n1, n2) -> n I.== n1 B.|| n I.== n2) es)

-- We don't seem to be able to [: a :] == [: a :]
-- Also we cannot toPArrayP x == toPArrayP y
-- Therefore here is some horrible code that compares each edge directly 
notEqualGraph :: Graph -> Graph -> Bool
notEqualGraph es fs = B.not (andP (mapP (\i -> equalEdge (indexP es i) (indexP fs i)) (enumFromToP 0 (lengthP es I.- 1))))

equalEdge :: Edge -> Edge -> Bool
equalEdge e@(n1, n2) f@(n3, n4) = n1 I.== n3 B.&& n2 I.== n4

-- Entrypoint. Provide a Graph in the form of PArray Edge and we return [Graph] in the form of PArray Edge
-- Owing to the limitations (and no toNestedPArrayP), we flatten the [[Edge]] to [Edge]
--    normally we would return a forest of trees.
kruskalPA :: PArray Edge -> PArray Edge
kruskalPA ps = toPArrayP (concatP (kruskal (fromPArrayP ps) emptyP))
{-# NOINLINE kruskalPA #-}

-- Kruskal's Minimum Spanning Tree Algorithm.
-- Given a list (or PArray) of edges sorted by weight, cheapest first,
--   pop each edge in turn
--     if both nodes in edge are new, create a new subtree
--     elif one node is new and the other is not, add the new node to the old node's subtree
--     elif both nodes exist in different subtrees, merge them
--     elif both nodes seen before, ignore this edge
-- Result: A forest of trees representing the MST.
kruskal :: [:Edge:] -> [:Graph:] -> [:Graph:]
kruskal es msts
  | lengthP es I.== 0 = msts
  | otherwise = kruskal (sliceP 1 (lengthP es I.- 1) es) (kruskal' msts (es !: 0))

---- The main logic. Check the status of the two nodes in an edge and act on the MSTs accordingly.
kruskal' :: [:Graph:] -> Edge -> [:Graph:]
kruskal' msts e@(n1,n2)
--  -- Both Nodes do not exist in any of the MST subtrees. Therefore create a new subtree
  | lengthP leftNodeMST I.== 0 && lengthP rightNodeMST I.== 0 = (singletonP (singletonP e)) +:+ msts
--  -- One node exists in a subtree but not the other. Add the other node to the subtree
  | lengthP leftNodeMST I.== 0  = singletonP ((singletonP e) +:+ rightNodeMST) +:+ (filterP (\mst -> notEqualGraph mst rightNodeMST) msts)
  | lengthP rightNodeMST I.== 0 = singletonP ((singletonP e) +:+ leftNodeMST) +:+ (filterP (\mst -> notEqualGraph mst leftNodeMST) msts)
--  -- Both nodes exist in different subtrees
  | (notEqualGraph leftNodeMST rightNodeMST) = singletonP ((singletonP e) +:+ (rightNodeMST+:+leftNodeMST)) +:+ (filterP (\mst -> (notEqualGraph mst leftNodeMST) B.&& (notEqualGraph mst rightNodeMST)) msts)
--  -- Both nodes already exist in the same subtree
  | otherwise = msts
  where
    leftNodeMST = nodeInMSTs msts n1
    rightNodeMST = nodeInMSTs msts n2

