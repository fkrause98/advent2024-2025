module Main where

import Data.Char
import Data.List
import Data.Map.Strict qualified as Map
import Debug.Trace
import Utils

import Control.Monad.State

type Cache = Map.Map Int Int

testInput :: [[Char]]
testInput = ["..@@.@@@@.", "@@@.@.@.@@", "@@@@@.@.@@", "@.@@@@..@.", "@@.@@@@.@@", ".@@@@@@@.@", ".@.@.@.@@@", "@.@@@.@@@@", ".@@@@@@@@.", "@.@.@@@.@."]

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
countRolls :: (Int, Int) -> [[Char]] -> Int
countRolls (x, y) grid =
    length $
        filter (== Just '@') $
            map
                ( \(u, v) ->
                    case grid !? u of
                        Just g -> g !? v
                        Nothing -> Nothing
                )
                dirs
  where
    dirs = map (\(u, v) -> (x + u, y + v)) deltas

main :: IO ()
main = do
    input <- lines <$> readFile "./input/day4_2025.txt"
    let dirs = filter (\(x, y) -> (== '@') $ (input !! x) !! y) $ [(x, y) | x <- [0 .. (length input) - 1], y <- [0 .. (length $ input !! x) - 1]]
    print $ length $ filter (< 4) $ map (\(x, y) -> countRolls (x, y) input) dirs
