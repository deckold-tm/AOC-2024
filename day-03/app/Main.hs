module Main where

import Control.Monad
import Control.Monad.Trans.Except (runExceptT)
import Data.Char (isDigit)
import Data.Either (fromRight, rights)
import Debug.Trace
import System.Environment
import System.IO.Error
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Printf (printf)
import Utils

main :: IO ()
main = do
  maybe_content <- runExceptT $ readFileFromArg getArgs
  case maybe_content of
    Left err | isUserError err -> putStrLn "Please provide a file path as an argument"
    Left err | isDoesNotExistError err -> putStrLn "No file found"
    Right content -> run $ content ++ "don't()do()"

run :: String -> IO ()
run content = do
  parsed_data <- return $ tryParse content
  putStrLn "The answer to part 1 is:"
  putStrLn $ sumResult parsed_data
  putStrLn $ "The anser to part 2 is:"
  parsed_data2 <- return $ parse conditionalComand "" content
  putStrLn $ show $ head $ rights [parsed_data2]

sumResult :: [Either a Int] -> String
sumResult = show . sum . rights

number :: Parser Int
number = do
  read <$> (try (count 3 digit) <|> try (count 2 digit) <|> try (count 1 digit))

command :: Parser ((Int), String)
command = do
  manyTill anyChar (try (string "mul("))
  a <- number
  string ","
  b <- number
  string ")"
  rest <- many anyChar
  return (a * b, rest)

tryParse :: String -> [Either ParseError Int]
tryParse content = case parse command "" content of
  Right (i, []) -> Right i : []
  Right (i, rest) -> Right i : tryParse rest
  Left err -> case content of
    [] -> Left err : []
    c -> (Left err) : tryParse (drop 1 c)

-- conditionalComand:: Parser Int
conditionalComand = do
  do_blocks <- many condition
  parsed_blocks <- return $ map tryParse do_blocks
  ans <- return $ map rights parsed_blocks
  return $ sum $ map sum $ ans

condition = do
  chars <- manyTill anyChar (try (string "don't()"))
  _ <- try $ manyTill anyChar (try ((string "do()")))
  return chars
