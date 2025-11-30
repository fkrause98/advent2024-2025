module Main where
import Data.List
import Data.Bifunctor()

part1 :: IO ()
part1 = do
  input <- readFile "./input/day2.txt"
  let levels = map words (lines input)
  let levelsInt = map (\lst -> (map (read) lst) :: [Integer]) levels
  let safeLevels = map isLevelSafe levelsInt
  let asNums = foldl (\total isSafeLevel -> if isSafeLevel then 1 + total else total) 0 safeLevels
  putStrLn $ "Part 1: " ++ show asNums


part2 :: IO ()
part2 = do
  input <- readFile "./input/day2.txt"
  let levels = map words (lines input)
  let levelsInt = map (\lst -> (map (read) lst) :: [Integer]) levels
  let safeLevels = map isLevelSafe2 levelsInt
  let asNums = foldl (\total isSafeLevel -> if isSafeLevel then 1 + total else total) 0 safeLevels
  putStrLn $ "Part 2: " ++ show asNums


allIncreasing :: [Integer] -> Bool
allIncreasing (x:y:ys) =
  x > y && allIncreasing (y:ys)
allIncreasing _ = True

allDecreasing :: [Integer] -> Bool
allDecreasing (x:y:ys) =
  x < y && allDecreasing (y:ys)
allDecreasing _ = True

diffsInRange :: [Integer] -> Bool
diffsInRange (x:y:ys) =
  (abs (x-y) >= 1) && (abs (x-y) <= 3) && diffsInRange (y:ys)
diffsInRange _ = True

isLevelSafe :: [Integer] -> Bool
isLevelSafe lst = (allIncreasing lst || allDecreasing lst) && diffsInRange lst

isLevelSafe2 :: [Integer] -> Bool
isLevelSafe2 =
  foldl (\isSafe acum -> isSafe || acum) False
  . map isLevelSafe
  . generateVariations

generateVariations :: [Integer] -> [[Integer]]
generateVariations lst = zipWith (++) (inits lst) (tail (tails lst))

main :: IO ()
main = do
  part1
  part2
