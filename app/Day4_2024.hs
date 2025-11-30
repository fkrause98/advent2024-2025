module Main where

import Data.List (transpose)
import Debug.Trace

main :: IO ()
main = part1

part1 :: IO ()
part1 = readFile "./input/day4.txt" >>= print . solvePart1 . lines

-- part2 :: IO ()
-- part2 = readFile "./input/day4.txt" >>= print . solvePart2 . lines


diagonals :: [[a]] -> [[a]]
diagonals =
  (++)
    <$> reverse . transpose . zipWith drop [0 ..]
    <*> transpose . zipWith drop [1 ..] . transpose

antiDiagonals :: [[a]] -> [[a]]
antiDiagonals = diagonals . map reverse


columns :: [[a]] -> [[a]]
columns = transpose

rows :: [[a]] -> [[a]]
rows s = s

countXmasInString :: [Char] -> Int
countXmasInString ('X':'M':'A':'S':xs) = 1 + countXmasInString ('M':'A':'S':xs)
countXmasInString ('S':'A':'M':'X':xs) = 1 + countXmasInString ('A':'M':'X':xs)
countXmasInString (_:xs)              = countXmasInString xs
countXmasInString []                  = 0

countPattern :: [ [ Char ] ] -> ([Char] -> Int) -> Int
countPattern input patternCounter  =  sum ( map patternCounter input )

solvePart1 :: [ String ] -> Int
solvePart1 s = countPattern allDirections countXmasInString
  where
    allDirections = (columns s) ++ (rows s) ++ ( diagonals s ) ++ (antiDiagonals s)

