module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.Monad.Trans.State.Strict
import Data.Array
import Data.List ( findIndices , elemIndex, findIndex, )
import Data.Maybe (fromJust)
import Text.Parsec (ParseError, char, many1, newline, oneOf, optional, parse, (<|>), )
import Text.Parsec.String
import Utils
import Debug.Trace

type Bounds = ((Int, Int), (Int, Int))

type Array2d a = Array (Int, Int) a

main :: IO ()
main = runTask myTask

myTask :: AOC
myTask content = do
  size <- mapSize content
  let mapList = parseMapCharacter content
  mapList' <- case mapList of
    Right m -> return m
    Left _ ->  undefined 
  parsedMap <- parseMap size mapList'
  return (
    -- (trace $ showMap $ runGuard parsedMap) $ sumVisited $ runGuard parsedMap, 
    sumVisited $ runGuard parsedMap, 
    Just $ show $ countLoops $ tryVisited parsedMap
    )

parseMap :: (Monad m) => Bounds -> [a] -> m (Array2d a)
parseMap b a = return $ listArray b a

sumObst :: Array i MapElement -> String
sumObst a = show $ length $ filter id $ map (== Obst) $ elems a
sumVisited :: Array i MapElement -> String
sumVisited a = show $ length $ filter id $ map isVisited $ elems a

mapSize :: (Monad m) => String -> m Bounds
mapSize content = do
  let ls = lines content 
  let height = length ls
  let width = length $ head ls
  return ((1, 1), (width, height))

-- data MapElement = Obst | Guard {orientation :: Orientation} | Empty EmptyState

data Orientation = North | East | South | West deriving (Eq, Enum, Bounded)

data MapElement = Obst | Guard {orientation :: Orientation} | Empty {status :: VisitedState, prevOrientations :: [Orientation]}

data VisitedState = UnVisited | Visited

-- data EmptyState = UnVisited | North' | East' | South' | West'

updateEmpty :: [Orientation] -> Orientation -> MapElement
updateEmpty cur dir = Empty Visited (dir:cur)
-- updateEmpty :: MapElement -> Orientation -> MapElement
-- updateEmpty cur dir = case cur of
--     Empty UnVisited _ -> Empty Visited [dir]
--     Empty Visited lst -> Empty Visited (dir:lst)
--     _ -> undefined


-- updateEmptyState :: Orientation -> EmptyState
-- updateEmptyState a = case a of
--   North -> North'
--   East -> East'
--   South -> South'
--   West -> West'

next :: Orientation -> Orientation
next a = if maxBound == a then minBound else succ a

type Map = Array ((Int, Int), (Int, Int)) MapElement

showMap :: (Show a) => Array2d a -> String
showMap m = let (l,u) = bounds m in unlines [unwords [show  $ m ! (y, x) | x<- [(snd l)..(snd u)]] | y <- [(fst l)..(fst u)]]


instance Eq MapElement where
  Obst == Obst = True
  Empty UnVisited _ == Empty UnVisited _ = True
  Empty _ _ == Empty _ _ = True
  Guard _ == Guard _ = True
  _ == _ = False

isVisited :: MapElement -> Bool
isVisited a = case a of
  Empty UnVisited _ -> False
  Empty Visited _ -> True
  Guard _ -> True
  _ -> False

isEmpty :: MapElement -> Bool
isEmpty a =case a of
  Empty _ _ -> True
  _ -> False


instance Show MapElement where
  show Obst = "#"
  show (Empty UnVisited _) = "."
  show (Empty Visited dir) = show $ length dir
  show (Guard North) = "^"
  show (Guard East) = ">"
  show (Guard South) = "v"
  show (Guard West) = "<"

parseMapCharacter :: String -> Either ParseError [MapElement]
parseMapCharacter = parse (many1 (parseEmpty <|> parseObstruction <|> parseGuard)) "Parse Map"

parseEmpty :: Parser MapElement
parseEmpty = Empty UnVisited [] <$ char '.' <* optional newline

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

moveGuard :: (Monad m) => [Orientation] -> StateT (Array2d MapElement) m ()
moveGuard carry_dir = do
  cur <- get
  let nextPlace = moveOrTurn carry_dir cur
  case nextPlace of
    Nothing -> return ()
    Just n -> case fromJust $ checkAhead cur of
      Obst -> put $ execState (moveGuard [] ) n
      Empty _ dir -> put $ execState (moveGuard dir) n
      _ -> undefined

runGuard :: Array2d MapElement -> Array2d MapElement
runGuard = execState $ moveGuard []

curGuard :: Array2d MapElement -> ((Int, Int), Orientation)
curGuard cur =
  let elements = elems cur
      guardIdx = indices cur !! fromJust ( elemIndex (Guard North) elements)
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
        North -> (h - 1, w)
        East -> (h, w + 1)
        South -> (h + 1, w)
        West -> (h, w - 1)
      in if inRange (bounds cur) n then return n else Nothing 

moveOrTurn :: [Orientation] -> Array2d MapElement -> Maybe (Array2d MapElement)
moveOrTurn carry_dir cur = do
  a <- checkAhead cur
  nextIdx <- ahead cur
  return $ case a of
    Obst -> cur // [let (idx, o) = curGuard cur in (idx, Guard $ next o)]
    Empty _ _ ->
      let 
        (idx, o) = curGuard cur
        in cur // [(idx, updateEmpty carry_dir o), (nextIdx, Guard o)]
    -- Empty Visited dir -> undefined
    Guard _ -> undefined

checkLoop :: Array2d MapElement ->  Bool
checkLoop =  evalState (moveGuardLoop2 [])

tryVisited ::Array2d MapElement -> [Bool]
tryVisited cur = let
    start = fromJust $ elemIndex (Guard North) (elems cur)
    comp = runGuard cur
    vals = findIndices  isVisited $ elems comp
    idxs = indices cur
    initMap = [cur // [(idxs !! i, Obst)] | i <- filter (/=start)  vals]
    in parMap rdeepseq checkLoop initMap

tryAll :: Array2d MapElement -> [Bool]
tryAll cur = let
  vals = findIndices isEmpty  $ elems cur
  idxs = indices cur 
  initMap = [cur // [(idxs !! i, Obst)] | i <- vals ]
  in parMap rdeepseq checkLoop initMap

countLoops :: [Bool] -> Int
countLoops i = length $ filter id i

moveGuardLoop2 :: (Monad m) => [Orientation] -> StateT (Array2d MapElement) m Bool
moveGuardLoop2 carry_state = do
  cur <- get
  let a =  checkAhead cur
  let nextPlace = moveOrTurn carry_state cur
  let (_, guard_dir) = curGuard cur
  case a of
    Nothing -> return False
    Just e -> case e of
      Obst -> do
        (s, m) <- return $ runState (moveGuardLoop2 []) (fromJust nextPlace) 
        put m
        return s
      Empty UnVisited dir -> do
        (s, m) <- return $ runState (moveGuardLoop2 dir) (fromJust nextPlace) 
        put m
        return s
      Empty Visited dir ->
        -- if guard_dir `elem` dir then (trace $ showMap cur) $ return True
        if guard_dir `elem` dir then  return True
      -- Empty _ dir -> 
      --   if sameDirection go dir then (trace $ show $ elems cur) $ return True
        else do
          (s, m) <- return $ runState (moveGuardLoop2 (dir++carry_state)) $ fromJust nextPlace
          put m
          return s
      _ -> undefined

-- sameDirection :: Orientation -> EmptyState -> Bool
-- sameDirection North North' = True
-- sameDirection South South' = True
-- sameDirection East East' = True
-- sameDirection West West' = True
-- sameDirection _ _ = False
