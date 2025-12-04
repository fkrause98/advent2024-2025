module Main where

import Data.Char
import Data.List
import Data.Map.Strict qualified as Map
import Debug.Trace
import Utils

import Control.Monad.State

type Cache = Map.Map Int Int

testInput :: [[Int]]
testInput =
    [ [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1]
    , [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9]
    , [2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8]
    , [8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1]
    ]

readInput :: IO [String]
readInput = lines <$> readFile "./input/day3_2025.txt"

parseInput :: [String] -> [[Int]]
parseInput s = map parseList s
  where
    parseList xs = map digitToInt xs

main :: IO ()
main = do
    parsedInput <- parseInput <$> readInput
    print $ "Part 1 test: " ++ (show $ part1 testInput)
    print $ "Part 2 test: " ++ (show $ part2 testInput)
    print $ "Part 1 input " ++ (show $ part1 parsedInput)
    print $ "Part 2 input " ++ (show $ part2 parsedInput)

part1 :: [[Int]] -> Int
part1 xs = sum $ map (\x -> runMemoized solver (2, x)) xs

part2 :: [[Int]] -> Int
part2 xs = sum $ map (\x -> runMemoized solver (12, x)) xs

solver :: (Int, [Int]) -> (State (Map.Map (Int, [Int]) Int) Int)
solver (0, _) = return 0
solver (k, []) = return 0
solver (k, (x : xs))
    | k > length (x : xs) = return 0
    | k == length (x : xs) = return $ fromDigits (x : xs)
    | otherwise = do
        let frontDigit = x * (10 ^ (k - 1))
        takeX <- memoized solver (k - 1, xs)
        skipX <- memoized solver (k, xs)
        return $ maximum [frontDigit + takeX, skipX]
