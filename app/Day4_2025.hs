module Main where

import Data.Char
import Data.List
import Data.Map.Strict qualified as Map
import Debug.Trace
import Utils

type Grid = Map.Map (Int, Int) Char

testInput :: [String]
testInput = ["..@@.@@@@.", "@@@.@.@.@@", "@@@@@.@.@@", "@.@@@@..@.", "@@.@@@@.@@", ".@@@@@@@.@", ".@.@.@.@@@", "@.@@@.@@@@", ".@@@@@@@@.", "@.@.@@@.@."]

deltas :: [(Int, Int)]
deltas =
    [ (-1, 0)
    , (-1, -1)
    , (-1, 1)
    , (0, -1)
    , (0, 1)
    , (1, 0)
    , (1, -1)
    , (1, 1)
    ]
countRolls :: (Int, Int) -> Grid -> Int
countRolls (x, y) grid =
    length $
        filter (== Just '@') $
            map
                (\(u, v) -> Map.lookup (u, v) grid)
                dirs
  where
    dirs = map (\(u, v) -> (x + u, y + v)) deltas

part1 :: [(Int, Int)] -> Grid -> Int
part1 dirs input = length $ filter (< 4) $ map (\(x, y) -> countRolls (x, y) input) dirs

removables :: Grid -> [(Int, Int)] -> [(Int, Int)]
removables m coords =
    let neighs (x, y) = map (addTuples (x, y)) deltas
        rolls (x, y) = filter (== Just '@') $ map (fetch m) $ neighs (x, y)
        isRemovable (x, y) = (length $ rolls (x, y)) < 4
     in filter isRemovable coords

removeRolls :: Grid -> [(Int, Int)] -> Grid
removeRolls = foldl' (\m c -> Map.insert c '.' m)

part2 :: Grid -> Int
part2 m = go toRemove
  where
    rolls = [(x, y) | (x, y) <- Map.keys m, Map.lookup (x, y) m == Just '@']
    toRemove = removables m rolls
    go [] = 0
    go xs = (length xs) + part2 (removeRolls m xs)

main :: IO ()
main = do
    input <- lines <$> readFile "./input/day4_2025.txt"
    let maxX = length input
    let maxY = length $ head $ take 1 input
    let grid = Map.fromList [((x, y), (input !! x) !! y) | x <- [0 .. (length input) - 1], y <- [0 .. (length $ input !! x) - 1]]
    let dirs = [(x, y) | (x, y) <- Map.keys grid, grid Map.!? (x, y) == Just '@']
    print $ "Part 1: " ++ (show $ part1 dirs grid)
    print $ "Part 2:" ++ (show $ part2 grid)
