module Main where
import Data.List
import Data.Bifunctor

part1 :: IO ()
part1 = do
  input <- readFile "./input/day1.txt"
  let (list1, list2) = createAndSortLists $ inputAsTuples input
  let distances = calcDistances list1 list2
  putStrLn $ "Part 1 solution: " ++ show (sum distances)

part2 :: IO ()
part2 = do
  input <- readFile "./input/day1.txt"
  let (list1, list2) = createAndSortLists $ inputAsTuples input
  let appearances = map (`calcAppearances` list2)  list1
  let scores = zipWith (*) list1 appearances
  putStrLn $ "Part 2 solution: " ++ show (sum scores)

main :: IO ()
main = do
  part1
  part2


calcAppearances :: Integer -> [Integer] -> Integer
calcAppearances x = fromIntegral . length . filter (== x)

calcDistances :: [Integer] -> [Integer] -> [Integer]
calcDistances = zipWith (\x y -> abs (x - y))

createAndSortLists :: [(Integer, Integer)] -> ([Integer], [Integer])
createAndSortLists tuplesInput = bimap sort sort (unzip tuplesInput)

inputAsTuples :: String -> [(Integer, Integer)]
inputAsTuples input = map lineToTuple (lines input)
  where
    lineToTuple line = case words line of
      [elem1, elem2] -> (read elem1, read elem2)
      _ -> error "Wrong input"
