module Utils where

import Control.Monad.State
import Data.Char (digitToInt)
import Data.List
import qualified Data.Map as M

diagonals :: [[a]] -> [[a]]
diagonals =
    (++)
        <$> reverse . transpose . zipWith drop [0 ..]
        <*> transpose . zipWith drop [1 ..] . transpose

antiDiagonals :: [[a]] -> [[a]]
antiDiagonals = diagonals . map reverse

data Direction = North | South | West | East
    deriving (Eq, Show)

turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight West = North
turnRight East = South

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f lst = case findIndex f lst of
    Just indx -> indx
    Nothing -> error "Could not find element"

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where
    addDigit num d = 10 * num + d

-- Taken from this blog post: https://angusjf.com/haskell-memoization/
-- Memoizes an unary function
memoized :: (Ord x) => (x -> State (M.Map x y) y) -> x -> State (M.Map x y) y
memoized f x = do
    cache <- get
    case M.lookup x cache of
        Just hit -> return hit
        Nothing -> do
            res <- f x
            modify (M.insert x res)
            return res

runMemoized :: (x -> State (M.Map x y) y) -> x -> y
runMemoized f x = evalState (f x) M.empty

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (u, v) = (x + u, y + v)

fetch :: (Ord k) => M.Map k v -> k -> Maybe v
fetch m x = M.lookup x m

buildGrid :: [[Int]] -> M.Map (Int, Int) Int
buildGrid input =
    M.fromList
        [ ((x, y), (input !! x) !! y) | x <- [0 .. (length input) - 1], y <- [0 .. (length $ input !! x) - 1]
        ]

deltas1 :: [(Int, Int)]
deltas1 =
    [ (-1, 0),
      (-1, -1),
      (-1, 1),
      (0, -1),
      (0, 1),
      (1, 0),
      (1, -1),
      (1, 1)
    ]

deltas2 :: [(Int, Int)]
deltas2 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

mapDeltas2 :: (Int, Int) -> [(Int, Int)]
mapDeltas2 t = map (addTuples t) deltas2

mapDeltas1 :: (Int, Int) -> [(Int, Int)]
mapDeltas1 t = map (addTuples t) deltas1

digitize :: String -> [Int]
digitize = map digitToInt
