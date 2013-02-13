module BGen (randomList) where
import Data.List
import System.Random

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)
