
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Matrix
import Data.List (findIndex)
import Data.Set (insert)
import qualified Data.Set as Set
import qualified Data.Matrix as Matrix
import Data.HashSet
import Data.Hashable
import GHC.Generics (Generic)


main :: IO ()
main = part2

part1 :: IO ()
part1 = do
  (charMatrix, guardState) <- buildMap <$> readFile "./input/day6.txt"
  let visited = walk guardState charMatrix (Set.singleton (position guardState))
  print $ Set.size visited

part2 :: IO ()
part2 = do
  (charMatrix, guardState) <- buildMap <$> readFile "./input/day6.txt"
  let loops = solvePart2 guardState charMatrix
  print $ loops



data Direction = North | South | West | East
  deriving (Eq, Show, Generic)
instance Hashable Direction

data GuardState = GuardState { position :: (Int, Int), dir :: Direction } deriving (Eq, Show)
type Visited = Set.Set (Int, Int)

turnRight :: GuardState -> Direction
turnRight GuardState { dir = North } = East
turnRight GuardState { dir = South } = West
turnRight GuardState { dir = West } = North
turnRight GuardState { dir = East } = South

increment :: GuardState -> (Int, Int)
increment GuardState {dir = North} = (-1, 0)
increment GuardState {dir = South} = (1, 0)
increment GuardState {dir = West} = (0, -1)
increment GuardState {dir = East} = (0, 1)

buildMap :: String -> (Matrix Char, GuardState)
buildMap rawMap = (fromLists mmap, GuardState { position = (guardRow + 1, guardColumn + 1), dir = North })
  where
    mmap = lines rawMap
    guardRow = findIndex' (\lst -> elem '^' lst) mmap
    guardColumn = findIndex' (\c -> c == '^') (mmap !! guardRow)

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f lst = case findIndex f lst of
  Just indx -> indx
  Nothing -> error "Could not find element"

walk :: GuardState -> Matrix Char -> Visited -> Visited
walk guard mmap visited =
    case safeGet newX newY mmap of
      Just '#' -> walk ( guard { dir = turnRight guard  } ) mmap visited
      Just _ -> walk (guard { position = (newX, newY) }) mmap (Data.Set.insert (currX, currY) visited)
      Nothing -> (Data.Set.insert (currX, currY) visited)
  where
    (x, y) = (increment guard)
    (currX, currY) = position guard
    (newX, newY) = (currX + x, currY + y)

markMap  :: Matrix Char -> Visited -> Matrix Char
markMap mmap visited =
    mapPos
      ( \(x, y) char ->
          if Set.member (x, y) visited  then
           'X'
           else
            char
      )
      mmap


solvePart2 :: GuardState -> Matrix Char -> Int
solvePart2 guard m = length $ Prelude.filter ( == True ) $ Prelude.map (walksIntoALoop guard Data.HashSet.empty) (mapsWithObstacles m)


mapsWithObstacles :: Matrix Char -> [Matrix Char]
mapsWithObstacles m = Prelude.map (generateMapWithNewObstacle m)  [(x, y) | x <- [1..(nrows m)], y <- [1..(ncols m)]]

generateMapWithNewObstacle :: Matrix Char -> (Int, Int) -> Matrix Char
generateMapWithNewObstacle m (x, y) =
  case safeGet x y m of
    Just '^' -> m
    Just '#' -> m
    Just _ -> Matrix.setElem '#' (x, y) m
    Nothing -> error "Out of bounds"


walksIntoALoop :: GuardState -> HashSet ((Int, Int), Direction) -> Matrix Char -> Bool
walksIntoALoop guard visited mmap
  | Data.HashSet.member (position guard, dir guard) visited = True
  | otherwise =
      let newVisited = Data.HashSet.insert (position guard, dir guard) visited
          (dx, dy)     = increment guard
          (currX, currY) = position guard
          (newX, newY)   = (currX + dx, currY + dy)
      in
        case safeGet newX newY mmap of
          Just '#' -> walksIntoALoop (guard { dir = turnRight guard }) newVisited mmap
          Just _   -> walksIntoALoop (guard { position = (newX, newY) }) newVisited mmap
          Nothing  -> False
