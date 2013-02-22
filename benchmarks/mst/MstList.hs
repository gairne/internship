type Node = Int
data Edge = E (Node, Node) Int
type Graph = [Edge]

instance Show Edge where
  show (E (u, v) w) = "(" ++ show u ++ ", " ++ show v ++ "):" ++ show w
instance Eq Edge where
  (==) (E (u, v) w) (E (y, z) x) = u == y && v == z && w == x
  (/=) (E (u, v) w) (E (y, z) x) = u /= y || v /= z || w /= x

test :: Graph
test = [E (4, 6) 1, E (3, 5) 2, E (1, 2) 4, E (1, 3) 6, E (2, 3) 9, E (4, 5) 15, E (3, 4) 16]

disjointTest :: Graph
disjointTest = [E (7, 8) 1, E (4, 6) 1, E (9, 10) 2, E (3, 5) 2, E (7, 9) 3, E (8, 10) 3, E (1, 2) 4, E (1, 3) 6, E (2, 3) 9, E (4, 5) 15, E (3, 4) 16]

sortByWeight :: [Edge] -> [Edge]
sortByWeight es = es--quickSort es

-- Find the sub MST that a node is in or [] if not
nodeInMSTs :: Node -> [Graph] -> Graph
nodeInMSTs n [] = []
nodeInMSTs n (mst:msts) = if (nodeInMST n mst) then mst else nodeInMSTs n msts

nodeInMST :: Node -> Graph -> Bool
nodeInMST n [] = False
nodeInMST n ((E e w):es)
  | n == (fst e) || n == (snd e) = True
  | otherwise = nodeInMST n es

kruskal :: Graph -> [Graph]
kruskal es = kruskal' (sortByWeight es) []

kruskal' :: [Edge] -> [Graph] -> [Graph]
kruskal' [] msts = msts
kruskal' ((E e w):es) msts
  -- Both Nodes do not exist in any of the MST subtrees. Therefore create a new subtree
  | leftNodeMST == [] && rightNodeMST == [] = kruskal' es ([E e w] : msts)
  -- One node exists in a subtree but not the other. Add the other node to the subtree
  | leftNodeMST == []  = kruskal' es (((E e w) : rightNodeMST) : (filter (/= rightNodeMST) msts))
  | rightNodeMST == [] = kruskal' es (((E e w) : leftNodeMST) : (filter (/= leftNodeMST) msts))
  -- Both nodes exist in different subtrees
  | leftNodeMST /= rightNodeMST = kruskal' es (((E e w) : (rightNodeMST++leftNodeMST)) : (filter (\x -> x /= leftNodeMST && x /= rightNodeMST) msts))
  -- Both nodes already exist in the same subtree
  | otherwise = kruskal' es msts
  where
    leftNodeMST = nodeInMSTs (fst e) msts
    rightNodeMST = nodeInMSTs (snd e) msts

main = putStrLn (show (kruskal disjointTest))
