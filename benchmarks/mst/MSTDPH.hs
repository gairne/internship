{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module MSTDPH (kruskalPA) where

import Data.Array.Parallel
import qualified Data.Array.Parallel.Prelude.Int as I

type Node = Int
type Edge = (Node, Node)
type Graph = [:Edge:]

--instance Show Edge where
--  show (E (u, v) w) = "(" ++ show u ++ ", " ++ show v ++ "):" ++ show w
--instance Eq Edge where
--  (==) (E (u, v) w) (y, z) = u == y && v == z && w == x
--  (/=) (E (u, v) w) (y, z) = u /= y || v /= z || w /= x

--instance Eq Graph where
--  (==) es fs = comparePArray es fs
--  (/=) es fs = not (comparePArray es fs)

--comparePArray :: [:Edge:] -> [:Edge:] -> Bool
--comparePArray [::] [::] = True
--comparePArray (: x : xs :) (: y : ys :) = x == y && comparePArray xs ys

-- Find the sub MST that a node is in or [] if not
nodeInMSTs :: [:Graph:] -> Node -> Graph
--nodeInMSTs [::] n = [::]
nodeInMSTs msts n = concatP $ mapP (\mst -> if (nodeInMST n mst) then mst else [::]) msts
--nodeInMSTs (mst:msts) n = if (nodeInMST n mst) then mst else nodeInMSTs n msts

nodeInMST :: Node -> Graph -> Bool
--nodeInMST n [::] = False
nodeInMST n es = orP (mapP (\e -> n == (fst e) || n == (snd e)) es)
--nodeInMST n (e:es) =
--  | n == (fst e) || n == (snd e) = True
--  | otherwise = nodeInMST n es

-- Entrypoint. Provide a Graph in the form of PArray Edge and we return [Graph] in the form of PArray Edge
-- Owing to the limitations (and no toNestedPArrayP), we flatten the [[Edge]] to [Edge]
--    normally we would return a forest of trees.
kruskalPA :: PArray Edge -> PArray Edge
kruskalPA ps = toPArrayP (concatP (kruskal (fromPArrayP ps) [::]))
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
kruskal [::] msts = msts
-- Hacky implementation of recursion
-- kruskal (e:es) msts = kruskal es (kruskal' msts e)
kruskal es msts = kruskal (sliceP 1 (lengthP es) es) (kruskal' msts (es !: 0))

-- The main logic. Check the status of the two nodes in an edge and act on the MSTs accordingly.
kruskal' :: [:Graph:] -> Edge -> [:Graph:]
kruskal' msts e
  -- Both Nodes do not exist in any of the MST subtrees. Therefore create a new subtree
  | lengthP leftNodeMST == 0 && lengthP rightNodeMST == 0 = (singletonP (singletonP e)) +:+ msts
  -- One node exists in a subtree but not the other. Add the other node to the subtree
  | lengthP leftNodeMST == 0  = singletonP ((singletonP e) +:+ rightNodeMST) +:+ (filterP (\mst -> (toPArrayP mst) /= (toPArrayP rightNodeMST)) msts)
  | lengthP rightNodeMST == 0 = singletonP ((singletonP e) +:+ leftNodeMST) +:+ (filterP (\mst -> (toPArrayP mst) /= (toPArrayP leftNodeMST)) msts)
  -- Both nodes exist in different subtrees
  | (toPArrayP leftNodeMST) /= (toPArrayP rightNodeMST) = singletonP ((singletonP e) +:+ (rightNodeMST+:+leftNodeMST)) +:+ (filterP (\mst -> (toPArrayP mst) /= (toPArrayP leftNodeMST) && (toPArrayP mst) /= (toPArrayP rightNodeMST)) msts)
  -- Both nodes already exist in the same subtree
  | otherwise = msts
  where
    [:leftNodeMST, rightNodeMST:] = mapP (nodeInMSTs msts) [: fst e, snd e :]
    --leftNodeMST = nodeInMSTs (fst e) msts
    --rightNodeMST = nodeInMSTs (snd e) msts

