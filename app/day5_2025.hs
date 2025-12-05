module Main where

import Data.List (partition, sort)

testInput =
    [ "3-5",
      "10-14",
      "16-20",
      "12-18",
      "1",
      "5",
      "8",
      "11",
      "17",
      "32"
    ]

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh db id = any (\(from, to) -> elem id [from .. to]) db

realInput = readFile "./input/day5_2025.txt"

parseRange :: String -> (Int, Int)
parseRange s =
    let
        x = (takeWhile (/= '-') s)
        y = drop 1 $ (dropWhile (/= '-') s)
     in
        (read x :: Int, read y :: Int)

part2 :: [(Int, Int)] -> Int
part2 xs = sum $ map (\(x, y) -> y - x + 1) $ foldl' mergeRanges [] $ sort xs
  where
    mergeRanges [] (x, y) = [(x, y)]
    mergeRanges ((x', y') : xs) (x, y)
        | x <= y' + 1 = (x', max y y') : (xs)
        | otherwise = (x, y) : (x', y') : xs

main :: IO ()
main = do
    input <- filter (not . null) <$> lines <$> realInput
    let (ranges, ids) = partition (\a -> elem '-' a) input
    print $ part2 $ map parseRange ranges
