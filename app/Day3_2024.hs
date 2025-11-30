module Main where
import Data.Bifunctor()
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Control.Monad.State
type Parser = Parsec Void String

leftParen :: Parser Char
leftParen = char '('
rightParen :: Parser Char
rightParen = char ')'
comma :: Parser Char
comma = char ','
mulInstruction :: Parser (Integer, Integer)
mulInstruction = do
  _ <- string "mul"
  _ <- leftParen
  x <- decimal
  _ <- comma
  y <- decimal
  _ <- rightParen
  return (x, y)

_do :: Parser Instruction
_do = do
  _ <- char 'd'
  _ <- char 'o'
  _ <- leftParen
  _ <- rightParen
  return Do

dont :: Parser Instruction
dont = do
  _ <- string "don't"
  _ <- leftParen
  _ <- rightParen
  return Dont

instruction :: Parser Instruction
instruction = try (_do) <|> try (dont) <|> (Mul <$> mulInstruction)


data Instruction = Mul (Integer, Integer) | Dont | Do deriving Show

extract :: Parser a -> Parser [a]
extract p = do (:) <$> try p <*> extract p
        <|> do anySingle >> extract p
        <|> do eof >> return []

getResult :: [(Integer, Integer)] -> Integer
getResult = sum . (map  mulTuple)
  where
    mulTuple (x, y) = x * y

part1 :: IO ()
part1 = do
  input <- readFile "./input/day3.txt"
  let matches = parse (extract mulInstruction) "" input
  case matches of
    Left err -> print $ errorBundlePretty err
    Right results -> print $ "Part 1 result: " ++ show (getResult results)


type MulState = (Bool, Integer)

dosAndDonts :: [Instruction] -> State MulState Integer
dosAndDonts [] = do
  (_, totalSum) <- get
  return totalSum
dosAndDonts (x:xs) = do
  (isEnabled, totalSum) <- get
  case x of
    Mul (a, b) | isEnabled -> put (isEnabled, totalSum + a*b)
    Mul _ -> put (isEnabled, totalSum)
    Dont -> put (False, totalSum)
    Do -> put (True, totalSum)
  dosAndDonts xs

part2 :: IO ()
part2 = do
  input <- readFile "./input/day3.txt"
  let matches = parse (extract instruction) "" input
  case matches of
    Left err -> print $ errorBundlePretty err
    Right results -> do
      let startState = (True, 0)
      print $ evalState (dosAndDonts results) startState
main :: IO ()
main = do
  part1
  part2
