module Main where

import Control.Concurrent (waitQSem)
import Control.Monad.Trans.Except (runExceptT)
import Data.Char (isDigit)
import Data.Either (fromRight, rights)
import System.Environment (getArgs)
import System.IO.Error (isDoesNotExistError, isUserError)
import Text.Parsec
  ( ParseError,
    anyChar,
    char,
    count,
    digit,
    eof,
    lookAhead,
    many1,
    manyTill,
    optional,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import Utils (readFileFromArg)

main :: IO ()
main = do
  maybe_content <- runExceptT $ readFileFromArg getArgs
  case maybe_content of
    Left err | isUserError err -> putStrLn "Please provide a file path as an argument"
    Left err | isDoesNotExistError err -> putStrLn "No file found"
    Right content -> run $ "do()" ++ content ++ "don't()"

run :: String -> IO ()
run content = do
  parsed_data <- return $ parse findMuls "" content
  putStrLn "The answer to part 1 is:"
  putStrLn $ show $ sumResult parsed_data
  putStrLn $ "The answer to part 2 is:"
  parsed_data2 <- return $ parse conditionalCommand "" content
  putStrLn $ show $ parsed_data2

sumResult :: Either ParseError [[Int]] -> Either ParseError Int
sumResult = fmap sum . fmap concat

number :: Parser Int
number = do
  read <$> ((try (count 3 digit)) <|> (try (count 2 digit)) <|> (try (count 1 digit)))

mul :: Parser Int
mul = (*) <$> ((string "mul(") *> number <* (char ',')) <*> (number <* (char ')'))

gobble1 :: Parser String
gobble1 = try (manyTill anyChar (try (lookAhead mul))) <|> manyTill anyChar eof

findMuls :: Parser [[Int]]
findMuls = gobble1 *> manyTill (many1 (try mul) <* try (optional gobble1)) eof

start :: Parser String
start = string "do()"

stop :: Parser String
stop = string "don't()"

gobble2 :: Parser String
gobble2 = try $ manyTill anyChar (try (lookAhead (start)))

condition :: Parser String
condition = try (start *> manyTill anyChar (try stop))

conditionalCommand :: Parser Int
conditionalCommand = do
  do_blocks <- manyTill (condition <* optional gobble2) eof
  parsed_blocks <- return $ rights $ fmap (parse findMuls "") do_blocks
  return $ sum $ (map sum . map concat) parsed_blocks
