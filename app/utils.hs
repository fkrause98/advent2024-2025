-- |

module Utils where

diagonals :: [[a]] -> [[a]]
diagonals =
  (++)
    <$> reverse . transpose . zipWith drop [0 ..]
    <*> transpose . zipWith drop [1 ..] . transpose

antiDiagonals :: [[a]] -> [[a]]
antiDiagonals = diagonals . map reverse


data Direction = North | South | West | East
  deriving (Eq, Show, Generic)


turnRight :: Directions -> Direction
turnRight North = East
turnRight South = West
turnRight West = North
turnRight East = South

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f lst = case findIndex f lst of
  Just indx -> indx
  Nothing -> error "Could not find element"
