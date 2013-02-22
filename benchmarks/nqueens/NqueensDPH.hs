{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module NqueensDPH (nqPA) where

import Data.Array.Parallel
import qualified Data.Array.Parallel.Prelude.Bool as B
import qualified Data.Array.Parallel.Prelude.Int as I

--nQueenToNSols = [(0, 0), (1, 1), (2, 0), (3, 0), (4, 2), (5, 10), (6, 4), (7, 40), (8, 92), (9, 352), (10, 724)]

type Solution  = [: (Int, Int) :]
type Solutions = [: Solution :]

-- We are testing whether putting a queen on row, col would conflict with any of the other previous queens
--  in this solution
-- For every queen we check against the proposed queen:
--   (1) that the columns are not the same
--   (2) that the queens aren't diagonal to each other
--
-- This function only works on an advancing wavefront. Assumes all previous queens are correct and safe
-- Also assumes that all queens are previous (in rows above).
correct :: Int -> Int -> Solution -> Bool
correct row col qs = andP (mapP (\q@(r,c) -> col I./= c B.&& col I./= c I.+ (row I.- r) B.&& col I./= c I.- (row I.- r)) qs)

queens :: Int -> Int -> Solutions -> Solutions
queens row cols solutions 
  | row I.== 0 = solutions
  | otherwise = queens (row I.- 1) cols [: (singletonP (row, col)) +:+ solution | col <- (I.enumFromToP 0 cols), solution <- solutions, correct row col solution :]

queensW :: Int -> Solutions
queensW n | n I.== 0    = emptyP
          | n I.== 1    = [:[:(0,0):]:]
          | n I.== 2    = emptyP
          | n I.== 3    = emptyP
          | otherwise = queensListComprehension (n I.- 1) (n I.- 1) [: [:(0, c):] | c <- I.enumFromToP 0 (n I.- 1) :]

nqPA :: Int -> Int
nqPA i = lengthP (queensW i)
{-# NOINLINE nqPA #-}

