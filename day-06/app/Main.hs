module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.Monad.Trans.State.Strict
import Data.Array
import Data.List ( findIndices , elemIndex, )
import Data.Maybe (fromJust)
import Text.Parsec (ParseError, char, many1, newline, oneOf, optional, parse, (<|>), )
import Text.Parsec.String
import Utils

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
    sumVisited $ runGuard parsedMap, 
    Just $ show $ countLoops $ tryAll parsedMap
    )

parseMap :: (Monad m) => Bounds -> [a] -> m (Array2d a)
parseMap b a = return $ listArray b a

sumObst :: Array i MapElement -> String
sumObst a = show $ length $ filter id $ map (== Obst) $ elems a
sumVisited :: Array i MapElement -> String
sumVisited a = show $ (+1) $ length $ filter id $ map isVisited $ elems a

mapSize :: (Monad m) => String -> m Bounds
mapSize content = do
  let ls = lines content 
  let height = length ls
  let width = length $ head ls
  return ((1, 1), (width, height))

data MapElement = Obst | Guard {orientation :: Orientation} | Empty {status ::  EmptyState}

data Orientation = North | East | South | West deriving (Eq, Enum, Bounded)

-- instance Hashable Orientation where
--   hashWithSalt = hashUsing fromEnum

data EmptyState = UnVisited | North' | East' | South' | West'

updateEmptyState :: Orientation -> EmptyState
updateEmptyState a = case a of
  North -> North'
  East -> East'
  South -> South'
  West -> West'

next :: Orientation -> Orientation
next a = if maxBound == a then minBound else succ a

type Map = Array ((Int, Int), (Int, Int)) MapElement

instance Eq MapElement where
  Obst == Obst = True
  -- Empty  == Empty b = 
    -- if (size a == 0) && (size b == 0) then True
    -- else if (size a >=1) && (size b >= 1) then True else False
  Empty UnVisited == Empty UnVisited = True
  Empty  _ == Empty  _ = True
  Guard _ == Guard _ = True
  _ == _ = False

isVisited :: MapElement -> Bool
isVisited a = case a of
  Empty UnVisited -> False
  Empty _ -> True
  _ -> False

isEmpty :: MapElement -> Bool
isEmpty a =case a of
  Empty _ -> True
  _ -> False

instance Show MapElement where
  show Obst = "#"
  show (Empty UnVisited) = "."
  show (Empty _) = "X"
  show (Guard North) = "^"
  show (Guard East) = ">"
  show (Guard South) = "v"
  show (Guard West) = "<"

parseMapCharacter :: String -> Either ParseError [MapElement]
parseMapCharacter = parse (many1 (parseEmpty <|> parseObstruction <|> parseGuard)) "Parse Map"

parseEmpty :: Parser MapElement
parseEmpty = Empty UnVisited <$ char '.' <* optional newline

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

moveGuard :: (Monad m) => StateT (Array2d MapElement) m ()
moveGuard = do
  cur <- get
  let nextPlace = moveOrTurn cur
  case nextPlace of
    Nothing -> return ()
    Just n -> put $ execState moveGuard n

runGuard :: Array2d MapElement -> Array2d MapElement
runGuard = execState  moveGuard

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

moveOrTurn :: Array2d MapElement -> Maybe (Array2d MapElement)
moveOrTurn cur = do
  a <- checkAhead cur
  nextIdx <- ahead cur
  return $ case a of
    Obst -> cur // [let (idx, o) = curGuard cur in (idx, Guard $ next o)]
    Empty _ ->
      let 
        (idx, o) = curGuard cur
        -- initHash = (insert o (empty )):: HashSet Orientation
        -- addedHash = insert o b
        in cur // [(idx, Empty (updateEmptyState o ::EmptyState)), (nextIdx, Guard o)]
    -- Empty a -> undefined
    Guard _ -> undefined

checkLoop :: (Array2d MapElement) ->  Bool
checkLoop = evalState moveGuardLoop2

tryAll :: Array2d MapElement -> [Bool]
tryAll cur = let
  vals = findIndices (\v -> (isEmpty) v || (isEmpty) v) $ elems cur
  idxs = indices cur 
  initMap = [cur // [(idxs !! i, Obst)] | i <- vals ]
  in parMap rdeepseq checkLoop $ initMap

countLoops :: [Bool] -> Int
countLoops i = length $ filter id i

-- moveGuardLoop :: (Monad m) => StateT (HashSet String,Array2d MapElement) m Bool
-- moveGuardLoop = do
--   (prev, cur) <- get
--   let nextPlace = moveOrTurn cur
--   let stateString = show $ fmap elems nextPlace
--   if member stateString prev then
--     case nextPlace of
--       Nothing -> return False
--       Just n -> do
--         (s, m) <- return $ runState moveGuardLoop (insert stateString prev, n)
--         put m
--         return s
--     else  return True

moveGuardLoop2 :: (Monad m) => StateT (Array2d MapElement) m Bool
moveGuardLoop2 = do
  cur <- get
  -- let nextPlace = moveOrTurn cur
  let a =  checkAhead cur
  let nextPlace = moveOrTurn cur
  let (_, go) = curGuard cur
  case a of
    Nothing -> return False
    Just e -> case e of
      Obst -> do
        (s, m) <- return $ runState moveGuardLoop2 $ fromJust nextPlace
        put m
        return s
      -- Empty UnVisited -> do
      --   (s, m) <- return $ runState moveGuardLoop2 cur
      --   put m
      --   return s
      Empty eo -> 
        if sameDirection go eo then return True
        else do
          (s, m) <- return $ runState moveGuardLoop2 $ fromJust nextPlace
          put m
          return s
      _ -> undefined

sameDirection :: Orientation -> EmptyState -> Bool
sameDirection North North' = True
sameDirection South South' = True
sameDirection East East' = True
sameDirection West West' = True
sameDirection _ _ = False
