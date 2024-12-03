module Main where

import Control.Exception (throw)
import Control.Monad.Trans
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe
import Data.Maybe (listToMaybe)
import System.Environment
import System.IO.Error (isDoesNotExistError, isUserError)
import Utils (readFileFromArg)

main :: IO ()
main = do
  maybe_content <- runExceptT $ readFileFromArg getArgs
  case maybe_content of
    Right content -> do
      run content
    Left err | isUserError err -> putStrLn "No file path given"
    Left err | isDoesNotExistError err -> putStrLn "File not found"
    Left err -> throw err

run :: String -> IO ()
run content = do
  parsedData <- getData content
  countSafe <- return (length $ filter isSafe $ map finiteDifference parsedData)
  countSafeDamped <- return $ length . filter id $ map tryDamped parsedData
  putStrLn "The answer to part 1 is:"
  putStrLn $ show countSafe
  putStrLn "The answer to part 2 is:"
  putStrLn $ show countSafeDamped

getFileName :: MaybeT IO String
getFileName = do
  args <- liftIO getArgs
  MaybeT $ return $ listToMaybe args

getData :: FilePath -> IO [[Int]]
getData = return . parse
  where
    parse = (map (map (read :: String -> Int))) . (map words) . lines

finiteDifference :: (Num a) => [a] -> [a]
finiteDifference x = [(x !! i) - (x !! (i - 1)) | i <- [1 .. len]]
  where
    len = (length x) - 1

isMonotonic :: (Num a, Ord a) => [a] -> Bool
isMonotonic x = (all (> 0) x) || (all (< 0) x)

isSmall :: (Num a, Ord a) => [a] -> Bool
isSmall x = all (<= 3) $ map abs x

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe x = isMonotonic x && isSmall x

tryDamped :: (Num a, Ord a) => [a] -> Bool
tryDamped x = any isSafe $ map finiteDifference [subset i x | i <- [0 .. len - 1]]
  where
    len = length x
    subset i x = let (h, t) = splitAt i x in h ++ drop 1 t
