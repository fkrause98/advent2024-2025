module Main where

import Data.Map (Map, fromList, toList, fromListWith, elems)

main :: IO ()
main = do
  part1
  part2

input :: Map Int Int
input = fromList $ Prelude.map (,1) [7725, 185, 2, 132869, 0, 1840437, 62, 26310]

part1 :: IO ()
part1 = do
  print $ "Part 1: " ++ (show $ sum $ Data.Map.elems $ blink 25 input)

part2 :: IO ()
part2 = do
  print $ "Part 2: " ++ (show $ sum $ Data.Map.elems $ blink 75 input)

updateStoneRules :: (Int, Int) -> [(Int, Int)]
updateStoneRules (stone, count)
  | stone == 0 = [(1, count)]
  | rem digitCount 2 == 0 = [(upperDigits, count), (lowerDigits, count)]
  | otherwise = [ (stone * 2024, count) ]
  where
    asString = Prelude.show stone
    digitCount = Prelude.length asString
    upperDigits = (read $ (take (digitCount `div` 2) asString)) :: Int
    lowerDigits = (read $ (drop (digitCount `div` 2) asString)) :: Int

blink :: Int -> (Map Int Int) -> (Map Int Int)
blink timesToBlink stones
  | timesToBlink == 0 = stones
  | otherwise = blink (timesToBlink - 1) updatedMap
  where
    newUpdates = concatMap updateStoneRules $ Data.Map.toList stones
    updatedMap = Data.Map.fromListWith (+) newUpdates
