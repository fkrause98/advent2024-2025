module Utils where

import Control.Monad.State
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
