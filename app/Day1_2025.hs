module Main where
import Data.List (mapAccumL)


main :: IO ()
main = do
  file <- lines <$> readFile "./input/day1_2025.txt"
  print $ part1 file
  print $ part2 file


part1 :: [String] -> Int
part1 = countClicks . rotateDial
  where
    countClicks = length . filter (== 0)
    rotateDial = scanl ((🄎) last) 50

part2 :: [String] -> Int
part2 = countZeroes . rotateDial
  where
    countZeroes = sum . snd
    rotateDial = mapAccumL oneRotation  50
    oneRotation start rots = ((🄎) last start rots , ( (🄎) (length . filter ( == 0 )) start rots) )

(🄎) :: ([Int] -> Int) -> Int -> String -> Int
(🄎) f start (rot:moves) = f $ take rotations $ dialStart
  where
    rotations = (read moves) :: Int
    dialStart = case rot of
      'R' -> drop (start + 1) $ cycle [0..99]
      'L' -> drop (100 - start) $ cycle [99,98..0]
