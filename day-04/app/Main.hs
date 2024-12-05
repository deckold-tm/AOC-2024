module Main where

import Control.Monad.Trans.Except
import Data.Array
import Debug.Trace
import System.Environment
import System.IO.Error
import Utils

main :: IO ()
main = do
  maybe_content <- runExceptT $ readFileFromArg getArgs
  case maybe_content of
    Left err | isUserError err -> putStrLn "Please provide a file path as an argument"
    Left err | isDoesNotExistError err -> putStrLn "No file found"
    Right content -> run content

data Size = Size {x :: Int, y :: Int} deriving (Show)

toArray :: String -> IO ()
toArray content = do
  size <- getSize content
  padded <- return $ map (\s -> "___" ++ s ++ "___") $ lines content
  padded' <- return $ (\p -> p ++ padded ++ p) (take 3 $ repeat (take (x size) (repeat '_')))
  matrix <- return $ listArray ((1, 1), (y size, x size)) (concat $ padded')
  putStrLn $ "The answer to part 1 is:"
  putStrLn $ show $ sum $ map fromEnum $ map matches $ concat $ getAll matrix
  putStrLn $ "The answer to part 2 is:"
  bools <- return  $ (map . map)  matchesMas $  getCross matrix
  putStrLn $ show $ sum $ map fromEnum $ map (all id) bools
  where
    -- putStrLn $ show $ padded'
    -- putStrLn $ show $ bools

    getSize :: String -> IO Size
    getSize content = do
      x <- return $ length . head $ lines content
      y <- return $ length $ lines content
      return $ Size {x = x + 6, y = y + 6}

run :: String -> IO ()
run = toArray

east :: (Num b, Enum b, Ix a1, Ix b) => Array (a1, b) a2 -> (a1, b) -> [a2]
east matrix (y, x) =
  [ matrix ! (y, x')
  | x' <- [x .. x + 3]
  ]

south :: (Num a1, Enum a1, Ix a1, Ix b) => Array (a1, b) a2 -> (a1, b) -> [a2]
south matrix (y, x) =
  [ matrix ! (y', x)
  | y' <- [y .. y + 3]
  ]

southeast :: (Num a1, Num b, Enum a1, Enum b, Ix a1, Ix b) => Array (a1, b) a2 -> (a1, b) -> [a2]
southeast matrix (y, x) =
  [ matrix ! (y', x')
  | (y', x') <- zip [y .. y + 3] [x .. x + 3]
  ]

northeast :: (Num a1, Num b, Enum a1, Enum b, Ix a1, Ix b) => Array (a1, b) a2 -> (a1, b) -> [a2]
northeast matrix ix = southeast matrix' ix
  where
    -- matrix' = ixmap (bounds matrix) (\i-> ((max_y+1 - fst i),( max_x+1 - snd i))) matrix
    matrix' = ixmap (bounds matrix) (\i -> ((fst i), (max_x + 1 - snd i))) matrix
    max_x = (snd $ snd $ bounds matrix)
    max_y = fst $ snd $ bounds matrix

getAll matrix = map (func matrix) (range $ bounds' matrix)
  where
    func matrix ix = [f matrix ix | f <- [east, south, southeast, northeast]]

bounds' matrix = (start, end)
  where
    start = ((fst $ fst $ bounds matrix) + 3, (snd $ fst $ bounds matrix) + 3)
    end = ((fst $ snd $ bounds matrix) - 3, (snd $ snd $ bounds matrix) - 3)

matches :: String -> Bool
matches s = (== "XMAS") s || (== (reverse "XMAS")) s

getCross :: (Num a, Num b, Enum a, Enum b, Ix a, Ix b) => Array (a, b) e -> [[[e]]]
getCross matrix = map (func matrix) (range $ bounds' matrix)
  where
    func matrix (y, x) = [f matrix (y, x) | f <- [getDiag, getDiag']]
    getDiag matrix (y, x) =
      [ matrix ! (y', x')
      | (y', x') <- zip [y - 1 .. y + 1] [x - 1 .. x + 1]
      ]
    getDiag' matrix (y, x) =
      [ matrix ! (y', x')
      | (y', x') <- zip [y + 1,y,y - 1] [x - 1 .. x + 1]
      ]

matchesMas :: String -> Bool
matchesMas s = (== "MAS") s || (== (reverse "MAS")) s
