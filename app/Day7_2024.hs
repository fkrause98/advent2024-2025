
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Debug.Trace
import Data.List.Split
import Data.List (foldl')

main :: IO ()
main = part1

part1 = do
  file <- readFile "./input/day7_test.txt"
  let input = parseInput file
  print $ sumSolvableEquations input

data Equation = Equation { leftSide :: Int, rightSide :: [Int] }
  deriving (Show)


buildEquation :: String -> Equation
buildEquation rawEquation = Equation { leftSide = read target, rightSide = parsedNums }
  where
    [target, nums] = splitOn ":" rawEquation
    parsedNums = map read $ words nums

parseInput :: String -> [Equation]
parseInput input = map buildEquation $ lines input

concatNums :: Int -> Int -> Int
x `concatNums` y =  read $ show x ++ show y

getPossibleResults :: [Int] -> [Int]
getPossibleResults [] = []
getPossibleResults (x:xs) = foldl' f [x] xs
  where
    f :: [Int] -> Int -> [Int]
    f currentVals nextNum =
      concatMap (\val -> [val + nextNum, val * nextNum, val `concatNums` nextNum]) currentVals

equationHolds :: Equation -> Bool
equationHolds Equation { leftSide, rightSide } =
  let possibleResults = getPossibleResults rightSide
  in elem leftSide possibleResults


sumSolvableEquations :: [Equation] -> Int
sumSolvableEquations eqs = sum $ map leftSide $ filter equationHolds eqs
