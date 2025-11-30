module Main where

import Data.Graph
import Data.Matrix
import Data.Char (digitToInt)
import GHC.Unicode (isDigit)
import Data.Maybe (catMaybes)
import Data.List (nub)


main = part1 *> part2

part1 = do
  matrix <- (matrixFromInput <$> readFile "./input/day10.txt")
  let solved =  solvePart1 <$> matrix
  print $ "part 1 " ++ (show solved)

part2 = do
  matrix <- (matrixFromInput <$> readFile "./input/day10.txt")
  let solved =  solvePart2 <$> matrix
  print $ "part 2 " ++ (show solved)

readDigit :: Char -> Maybe Int
readDigit c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

matrixFromInput :: String -> Maybe (Matrix Int)
matrixFromInput s = fromLists <$> inputAsDigits
  where
    inputAsDigits = traverse parseList $ lines s
    parseList = traverse readDigit



pathsToTake :: (Int, Int) -> Matrix Int -> [(Int, Int)]
pathsToTake (i, j) m = catMaybes $ map (\(x, y) -> safeGet x y m >> Just (x, y)) [ upperPath, lowerPath, leftPath, rightPath ]
  where
    upperPath = (i - 1, j)
    lowerPath = (i + 1, j)
    leftPath =  (i, j - 1)
    rightPath = (i, j + 1)


walkPaths :: (Int, Int) -> Matrix Int -> [[Int]]
walkPaths start m
  | currVal == 9 = [[9]]
  | otherwise = concatMap registerAndWalk walkablePaths
  where
    (i, j) = start
    currVal = unsafeGet i j m
    potentialPaths = pathsToTake start m
    walkablePaths = [ (x, y) | (x, y) <- potentialPaths, (currVal + 1) == (unsafeGet x y m) ]
    registerAndWalk next = map (currVal :) (walkPaths next m)

findReachableNines :: (Int, Int) -> Matrix Int -> [(Int, Int)]
findReachableNines start m
  | currVal == 9 = [start]
  | otherwise = concatMap registerAndWalk walkablePaths
  where
    (i, j) = start
    currVal = unsafeGet i j m
    potentialPaths = pathsToTake start m
    walkablePaths = [ (x, y) | (x, y) <- potentialPaths, (currVal + 1) == (unsafeGet x y m) ]
    registerAndWalk next = findReachableNines next m


solvePart1 :: Matrix Int -> Int
solvePart1 m = sumScores
  where
    possibleStarts = [ (x, y) | x <- [1..nrows m], y <- [1..ncols m], (unsafeGet x y m) == 0 ]

    allReachableNines :: [[(Int, Int)]]
    allReachableNines = map (\start -> findReachableNines start m) possibleStarts

    sumScores = sum $ map (length . nub) allReachableNines


solvePart2 :: Matrix Int -> Int
solvePart2 m = sumScores
  where
    possibleStarts = [ (x, y) | x <- [1..nrows m], y <- [1..ncols m], (unsafeGet x y m) == 0 ]

    allReachableNines :: [[(Int, Int)]]
    allReachableNines = map (\start -> findReachableNines start m) possibleStarts

    sumScores = sum $ map length  allReachableNines
