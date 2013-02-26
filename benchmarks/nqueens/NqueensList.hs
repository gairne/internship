module NqueensList (nqList) where

--nQueenToNSols = [(0, 0), (1, 1), (2, 0), (3, 0), (4, 2), (5, 10), (6, 4), (7, 40), (8, 92), (9, 352), (10, 724)]

type Solution  = [ (Int, Int) ]
type Solutions = [ Solution ]

-- We are testing whether putting a queen on row, col would conflict with any of the other previous queens
--  in this solution
-- For every queen we check against the proposed queen:
--   (1) that the columns are not the same
--   (2) that the queens aren't diagonal to each other
--
-- This function only works on an advancing wavefront. Assumes all previous queens are correct and safe
-- Also assumes that all queens are previous (in rows above).
correct :: Int -> Int -> Solution -> Bool
correct row col qs = and (map (\q@(r,c) -> col /= c && col /= c + (row - r) && col /= c - (row - r)) qs)

queens :: Int -> Int -> Solutions -> Solutions
queens row cols solutions 
  | row == 0 = solutions
  | otherwise = queens (row-1) cols [[(row, col)] ++ solution | col <- [0..cols], solution <- solutions, correct row col solution]

queensW :: Int -> Solutions
queensW n | n == 0    = []
          | n == 1    = [[(0,0)]]
          | n == 2    = []
          | n == 3    = []
          | otherwise = queens (n-1) (n-1) [[(0, c)] | c <- [0..(n-1)]]

nqList :: Int -> Int
nqList i = length (queensW i)

