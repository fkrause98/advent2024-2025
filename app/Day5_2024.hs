module Main where

import Data.List.Split (splitOn)
import Data.HashMap.Strict
import qualified Data.HashMap.Internal.Strict as Data
import Debug.Trace
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sortBy)

mmap = Prelude.map
ffilter = Prelude.filter

getKey hash key = (Data.HashMap.Strict.lookup) hash key

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  rules <- ( buildRulesMap . parseRules ) <$> readFile "./input/day5_rules.txt"
  updates <- parseUpdates <$> readFile "./input/day5_updates.txt "
  let solution = solvePart1 updates rules
  print $ "Part1: " ++ show (sum . (mmap middleElement) $ solution)

part2 :: IO ()
part2 = do
  rules <- ( buildRulesMap . parseRules ) <$> readFile "./input/day5_rules.txt"
  updates <- parseUpdates <$> readFile "./input/day5_updates.txt "
  let solution = solvePart2 updates rules
  print $ "Part2: " ++ show (sum . (mmap middleElement) $ solution)



buildRulesMap :: [[Int]] ->  Data.HashMap Int [Int]
buildRulesMap rules = fromListWith (++) [ (k, [v]) | [k, v] <- rules ]

parseRuleTuple :: [String] -> [Int]
parseRuleTuple [x,y] = [read x, read y]
parseRuleTuple _ = error "Wrong tuple passed"

filterIfEmpty :: [[a]] -> [[a]]
filterIfEmpty = ffilter (\xs -> (length xs) > 1)

parseRules :: String -> [[Int]]
parseRules =  (mmap parseRuleTuple) . filterIfEmpty . (mmap unwrapRule) . lines
  where
    unwrapRule = splitOn "|"

parseUpdates :: String -> [[Int]]
parseUpdates updates  = [ mmap read numList  | numList <- rawLines  ]
  where
    rawLines = (splitAtComma) .  filterIfEmpty . lines $ updates
    splitAtComma = mmap $ \x -> splitOn "," x


isValidUpdate :: [Int] -> Data.HashMap Int [Int] -> Bool
isValidUpdate [_] _ = True
isValidUpdate (x:xs) rules = (respectsOrder x xs) && (nonBefore x xs) && (isValidUpdate xs rules)
  where
    respectsOrder toTest nums = all (\ruleSet -> elem toTest ruleSet) $  numsToRules nums
    numsToRules = catMaybes . (mmap $ \num -> getKey num rules)
    nonBefore toTest nums  = case getKey toTest rules of
      Just ruleSet -> all (\num -> not (elem num ruleSet)) $ nums
      Nothing -> True

solvePart1 :: [[Int]] -> Data.HashMap Int [Int] -> [[Int]]
solvePart1 updates rules = ( ffilter checkIfValidUpdate) updates
  where
    checkIfValidUpdate = \x -> isValidUpdate (reverse x) (rules)

middleElement :: [Int] -> Int
middleElement [] = 0
middleElement xs = xs !! midIndex
  where
    len = length xs
    midIndex = (len - 1) `div` 2

sortBasedOnRules :: [Int] -> HashMap Int [Int] -> [Int]
sortBasedOnRules updateList rules = sortBy compareBasedOnRules updateList
  where
    rulesFor :: Int -> [Int]
    rulesFor k = fromMaybe [] (getKey k rules)
    compareBasedOnRules :: Int -> Int -> Ordering
    compareBasedOnRules x y
      | y `elem` rulesFor x = GT
      | x `elem` rulesFor y = LT
      | otherwise           = EQ

solvePart2 :: [[Int]] -> Data.HashMap Int [Int] -> [[Int]]
solvePart2 updates rules = mmap sortIt $ (ffilter checkIfNonValidUpdate) $ updates
  where
    sortIt xs = sortBasedOnRules xs rules
    checkIfNonValidUpdate = \x -> not (isValidUpdate (reverse x) (rules))
