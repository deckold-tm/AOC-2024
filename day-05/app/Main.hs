module Main where

import Control.Monad.Trans.Except (runExceptT)
-- import Control.Monad.Trans.State (runState)
import Control.Monad.Trans.State ( runState , State, get, put, execState, evalState)
import Data.Either ( rights ,lefts)
import Data.List ( elemIndex )
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Parsec
    ( char,
      digit,
      newline,
      count,
      manyTill,
      sepBy1,
      many1,
      parse,
      try )
import Text.Parsec.String ( Parser )
import Utils ( readFileFromArg )
import Debug.Trace

main :: IO ()
main = do
  args <- return getArgs
  maybe_content <- runExceptT $ Utils.readFileFromArg args
  case maybe_content of
    Left err -> putStrLn $ show err
    Right content -> runAOC content

runAOC :: String -> IO ()
runAOC content = do
  try_task <- return $ parse parseFile "" content
  case try_task of
    Left err -> putStrLn $ show err
    Right task -> do
      (ans1, ans2) <- runTask task
      putStrLn "The answer to part1 is:"
      putStrLn ans1
      putStrLn "The answer to part2 is:"
      putStrLn ans2

-- region parser

data Pair a = Pair {left :: a, right :: a} deriving (Show)

data Task a = Task {instList :: [Pair a], pages :: [[a]]} deriving (Show)

number :: Parser Int
number = (read :: String -> Int) <$> (many1 digit)

instructions :: Parser (Pair Int)
instructions = Pair <$> number <* (char '|') <*> number <* (newline)

pageList :: Parser [[Int]]
pageList = many1 $ sepBy1 number (char ',') <* newline

parseInstructions :: Parser [Pair Int]
parseInstructions = manyTill instructions (try ((count 1 newline)))

parseFile :: Parser (Task Int)
parseFile = Task <$> parseInstructions <*> pageList

-- end region

-- region task

runTask :: Task Int -> IO (String, String)
runTask task = do
  inst <- return $ instList task
  pageList <- return $ pages task
  ans1 <- mapM (checkPage inst) pageList
  return $ (show $ sum (rights ans1), show $ sum (lefts ans1))

checkPage :: (Monad m) => [Pair Int] -> [Int] -> m (Either Int Int)
checkPage insts p = do
  relaventInstructions <- return $ filter (\i -> (left i `elem` p) && (right i `elem` p)) insts
  isCorrect <- return $ all id $ map (\i -> not $ isOutOfOrder i p) relaventInstructions
  case isCorrect of
    True -> return $ Right (p !! (midpoint p))
    False -> return $ Left $ (p' !! (midpoint p))
      where
        nowCorrect p_= all id $ map (\i -> not $ isOutOfOrder i p_) relaventInstructions
        loopSwap p = if nowCorrect p then p else loopSwap $ swapAllForPage relaventInstructions p
        p' = loopSwap p
  where
    midpoint l = ((length l)) `div` 2

-- swapPages :: (Ord a, State m) => Pair a -> [a] -> m [a]
swapPages i = do
  p <- get
  case (isOutOfOrder i p) of
    False -> return ()
    True -> do
      p' <- get
      put $ swap' i p'
      where
        swap' i p =
          let l = fromJust (elemIndex (right i) p)
              r = fromJust (elemIndex (left i) p)
              lv = p !! l
              rv = p !! r
              h = take l p
              m = take (r - l - 1) (drop (l + 1) p)
              t = drop (r + 1) p
              new =  h ++ [rv] ++ m ++ [lv] ++ t
            in  new

isOutOfOrder :: (Eq a) => Pair a -> [a] -> Bool
isOutOfOrder i p = (fromJust $ elemIndex (left i) p) > (fromJust $ elemIndex (right i) p)

-- swapAllPages :: [Pair a] -> [a] -> Control.Monad.Trans.State.State s [a]
-- swapAllForPage inst p = execState (map (\i-> swapPages i p)) inst

swapAllForPage inst p = execState (sequence$ map (\i ->swapPages i) inst) p
--     let

-- end region
