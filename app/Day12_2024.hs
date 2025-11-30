module Main where

import Data.Graph
import Data.Tree
import qualified Data.Set as Set
import Data.Matrix hiding ((!))
import qualified Data.Matrix as M
import Data.Array ((!))

main :: IO ()
main = part1

part1 = do
  file <- lines <$> readFile "./input/day11.txt"
  let plotsMap = fromLists $ enumerate file
  let edges = edgesFromMatrix plotsMap
  let (graph, _, _) = graphFromEdges edges
  let plots = components graph
  let areas = map length plots
  let perimeters = map (countPerimeter graph) plots
  putStrLn $ ("Areas: " ++ (show areas))
  putStrLn $ ("Perimeters: " ++ (show perimeters))
  let prices = map (\(a, p) -> a*p) $ zip areas perimeters
  putStrLn $ "Prices: " ++ (show prices)
  putStrLn $ "Total: " ++ (show $ sum prices)

treeVertices :: Tree Vertex -> [Vertex]
treeVertices = foldMap (:[])

countPerimeter :: Graph -> Tree Vertex -> Int
countPerimeter graph component = sum $ map countMissingEdges componentList
  where
    componentVertices = Set.fromList $ treeVertices component
    componentList = Set.toList componentVertices

    countMissingEdges v =
      let neighbors = graph ! v
          neighborsInComponent = filter (`Set.member` componentVertices) neighbors
      in 4 - length neighborsInComponent

enumerate :: [String] -> [[(Int, Char)]]
enumerate = enumerateFrom 0

enumerateFrom :: Int -> [String] -> [[(Int, Char)]]
enumerateFrom _ [] = []
enumerateFrom i (string:strings) = enumeratedRow : enumerateFrom newStart strings
  where
    enumeratedRow = zip [i..] string
    newStart = i + length string

edgesFromMatrix :: Matrix (Int, Char) -> [(Char, Int, [Int])]
edgesFromMatrix m = toList matrixWithNeighbors
  where
    matrixWithNeighbors = mapPos buildGraphNodeWithNeighbors m
    buildGraphNodeWithNeighbors (i, j) (vertex, char) =
      (char, vertex, neighbors)
      where
        neighbors =
          [ neighborVertex | Just (neighborVertex, neighborChar) <- coords, char == neighborChar ]
        coords = map (\(k,l) -> safeGet k l m) [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]

