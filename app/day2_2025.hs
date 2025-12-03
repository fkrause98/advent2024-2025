-- |

{-# LANGUAGE OverloadedStrings #-}

module Main where
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import qualified Data.Text as T
import Data.Text (chunksOf)
import Debug.Trace
type Parser = Parsec Void String
type PError = ParseErrorBundle String Void
type ParseResult t = Either (PError) (Parser t)

range :: Parser (Int, Int)
range = do
  from <- decimal
  char '-'
  to <- decimal
  return (from, to)

inputParser :: Parser [(Int, Int)]
inputParser = range `sepBy` char ','

main :: IO ()
main = do
  print $ part1 part1Test
  let file = "./input/day_2_2025.txt"
  Right parsed <- runParser inputParser file <$> readFile file
  print $ part1 parsed

invalidId :: Int -> Bool
invalidId x = result
  where
    result = any (repeatsIn s) perms
    perms = drop 1 $ T.tails s
    s = T.show x

invalidIds :: (Int, Int) -> [Int]
invalidIds (from, to) = filter invalidId [from..to]

part1 :: [(Int,Int)] -> Int
part1 = sum . map (sum . invalidIds)

part1Test :: [(Int, Int)]
part1Test =
  [(11, 22)
  ,(95,115),(998, 1012),
   (1188511880, 1188511890),(222220, 222224),
    (1698522, 1698528),(446443, 446449),(38593856, 38593862),(565653, 565659),
    (824824821, 824824827), (2121212118, 2121212124)
  ]

repeatsIn :: T.Text -> T.Text -> Bool
repeatsIn digits pattrn
  | null digits' = False
  | otherwise = all (==pattrn) digits'
  where
    digits' = chunksOf (T.length pattrn) digits
