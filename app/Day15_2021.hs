module Main where

import Utils

import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.Map qualified as M

main :: IO ()
main = do
    input <- map digitize <$> lines <$> readFile "./input/day15_2021.txt"
    let
        grid = buildGrid input
        nodes = zip [0 ..] (M.keys grid)
        coordToNode = M.fromList [(coord, v) | (v, coord) <- nodes]
        edges = concatMap f nodes
        f (v, (x, y)) =
            let neighs = mapDeltas2 (x, y)
             in [ (v, v', p) | (x', y') <- neighs, Just p <- [M.lookup (x', y') grid], Just v' <- [M.lookup (x', y') coordToNode]
                ]
        graph = G.mkGraph nodes edges :: Gr (Int, Int) Int
        endingNode = (length nodes) - 1
        path = spLength 0 endingNode graph
    print $ path

testInput :: [[Int]]
testInput =
    [ [1, 1, 6, 3, 7, 5, 1, 7, 4, 2],
      [1, 3, 8, 1, 3, 7, 3, 6, 7, 2],
      [2, 1, 3, 6, 5, 1, 1, 3, 2, 8],
      [3, 6, 9, 4, 9, 3, 1, 5, 6, 9],
      [7, 4, 6, 3, 4, 1, 7, 1, 1, 1],
      [1, 3, 1, 9, 1, 2, 8, 1, 3, 7],
      [1, 3, 5, 9, 9, 1, 2, 4, 2, 1],
      [3, 1, 2, 5, 4, 2, 1, 6, 3, 9],
      [1, 2, 9, 3, 1, 3, 8, 5, 2, 1],
      [2, 3, 1, 1, 9, 4, 4, 5, 8, 1]
    ]
