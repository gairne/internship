testCorrect1 :: Bool
testCorrect1 = (correct 0 2 [(0,0), (1,4)]) == False
testCorrect2 :: Bool
testCorrect2 = (correct 1 2 [(0,0), (1,4)]) == True
testCorrect3 :: Bool
testCorrect3 = (correct 2 2 [(0,0), (1,4)]) == False
testCorrect4 :: Bool
testCorrect4 = (correct 3 2 [(0,0), (1,4)]) == False

testCorrect5 :: Bool
testCorrect5 = (correct 0 3 [(0,1), (1,3), (2,0)]) == False
testCorrect6 :: Bool
testCorrect6 = (correct 1 3 [(0,1), (1,3), (2,0)]) == False
testCorrect7 :: Bool
testCorrect7 = (correct 2 3 [(0,1), (1,3), (2,0)]) == True
testCorrect8 :: Bool
testCorrect8 = (correct 3 3 [(0,1), (1,3), (2,0)]) == False
testCorrect9 :: Bool
testCorrect9 = (correct 4 3 [(0,1), (1,3), (2,0)]) == False
testCorrectA :: Bool
testCorrectA = (correct 5 3 [(0,1), (1,3), (2,0)]) == False
testCorrectB :: Bool
testCorrectB = (correct 6 3 [(0,1), (1,3), (2,0)]) == True


correct :: Int -> Int -> [(Int, Int)] -> Bool
correct col row qs = and (map (\q@(r,c) -> col /= c && col /= c + (row - r) && col /= c - (row - r)) qs)

nextQueen :: Int -> Int -> [[(Int, Int)]] -> [[(Int, Int)]]
nextQueen row cols curGrids = [ (row, col):curGrid | col <- (enumFromTo 0 cols), curGrid <- curGrids, correct col row curGrid]

queens :: Int -> Int -> [[(Int, Int)]] -> [[(Int, Int)]] 
queens ncols currow c 
  | currow == ncols = nextQueen currow ncols c
  | otherwise       = queens ncols (currow + 1) (nextQueen currow ncols c)

queensW :: Int -> [[(Int, Int)]]
queensW n = queens (n-1) 1 [[(0, c)] | c <- [0..(n-1)]]

main = putStrLn (show (length(queensW 8)))
