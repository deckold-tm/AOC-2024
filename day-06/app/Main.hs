module Main where

import Control.Exception
import Control.Monad.Trans.State.Lazy
import Data.Array
import Data.Ix (inRange)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Debug.Trace
import Text.Parsec (ParseError, char, many1, newline, oneOf, optional, parse, (<|>))
import Text.Parsec.String
import Utils

type Bounds = ((Int, Int), (Int, Int))

type Array2d a = Array (Int, Int) a

main :: IO ()
main = runTask myTask

myTask :: AOC
myTask content = do
  size <- mapSize content
  mapList <- return $ parseMapCharacter content
  mapList <- case mapList of
    Right m -> return m
    Left err -> throw err
  parsedMap <- parseMap size mapList
  -- return $ (show  parsedMap, Nothing)
  return (sumVisited $ runGuard parsedMap, Nothing)

parseMap :: (Monad m) => Bounds -> [a] -> m (Array2d a)
parseMap b a = return $ listArray b a

-- sumObst :: Monad m => Array s MapElement -> m Int
-- sumObst a = do
--     v <- return $ elems a
--     obsts <- return $ filter id $  map (==Obst) v
--     return $ length obsts
sumObst :: Array i MapElement -> String
sumObst a = show $ length $ filter id $ map (== Obst) $ elems a
sumVisited :: Array i MapElement -> String
sumVisited a = show $ (+1) $ length $ filter id $ map (== Empty True) $ elems a

mapSize :: (Monad m) => String -> m Bounds
mapSize content = do
  ls <- return $ lines content
  height <- return $ length ls
  width <- return $ length $ head ls
  return ((1, 1), (width, height))

data MapElement = Obst | Guard {orientation :: Orientation} | Empty {isVisted :: Bool}

data Orientation = North | East | South | West deriving (Eq, Enum, Bounded)

next :: Orientation -> Orientation
next a = if maxBound == a then minBound else succ a

type Map = Array ((Int, Int), (Int, Int)) MapElement

instance Eq MapElement where
  Obst == Obst = True
  Empty False == Empty False = True
  Empty True == Empty True = True
  Guard _ == Guard _ = True
  _ == _ = False

instance Show MapElement where
  show Obst = "#"
  show (Empty False) = "."
  show (Empty True) = "X"
  show (Guard _) = "g"

parseMapCharacter :: String -> Either ParseError [MapElement]
parseMapCharacter = parse (many1 (parseEmpty <|> parseObstruction <|> parseGuard)) "Parse Map"

parseEmpty :: Parser MapElement
parseEmpty = Empty False <$ char '.' <* optional newline

parseObstruction :: Parser MapElement
parseObstruction = Obst <$ char '#' <* optional newline

parseGuard :: Parser MapElement
parseGuard = oneOf "^>v<" <* optional newline >>= mapDirection

mapDirection :: (Monad m) => Char -> m MapElement
mapDirection c = case c of
  '^' -> return $ Guard North
  '>' -> return $ Guard East
  'v' -> return $ Guard South
  '<' -> return $ Guard West
  _ -> undefined

moveGuard :: (Monad m) => StateT ((Array2d MapElement)) m ()
moveGuard = do
  cur <- get
  nextPlace <- return $ moveOrTurn cur
  case nextPlace of
    Nothing -> return ()
    Just n -> put $ execState moveGuard n

runGuard = execState $  moveGuard

curGuard :: Array2d MapElement -> ((Int, Int), Orientation)
curGuard cur =
  let elements = elems cur
      guardIdx = indices cur !! (fromJust $ findIndex (== (Guard North)) elements)
      orient = orientation $ cur ! guardIdx
   in (guardIdx, orient)

checkAhead :: Array2d MapElement -> Maybe MapElement
checkAhead cur = do
  new <- ahead cur
  return $ cur ! new

ahead :: Array2d MapElement -> Maybe (Int, Int)
ahead cur =
  let ((h, w), orient) = curGuard cur
      n = case orient of
        North -> ((h - 1), w)
        East -> (h, w + 1)
        South -> (h + 1, w)
        West -> (h, w - 1)
   in case inRange (bounds cur) n of
        True -> return n
        False -> Nothing

moveOrTurn :: Array2d MapElement -> Maybe (Array2d MapElement)
moveOrTurn cur = do
  a <- checkAhead cur
  nextIdx <- ahead cur
  return $ case a of
    Obst -> cur // [let (idx, o) = curGuard cur in (idx, Guard $ next o)]
    Empty _ ->
      let (idx, o) = curGuard cur
       in cur // [(idx, Empty True), (nextIdx, Guard o)]
    Guard _ -> undefined
