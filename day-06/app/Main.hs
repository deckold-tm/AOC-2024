module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.Monad.Trans.State.Strict
import Data.Array
import Data.List ( findIndices , elemIndex, find , uncons)
import Data.Maybe (fromJust )
-- import Data.Either
import Text.Parsec (ParseError, char, many1, newline, oneOf, optional, parse, (<|>))
import Text.Parsec.String
import Utils
import Debug.Trace

max_iter :: Int
max_iter = 15000 :: Int

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
  let width = length $ fromJust $ fst <$> uncons ls
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

instance Show Orientation where
  show North = "^"
  show East = ">"
  show South = "v"
  show West = "<"

instance Show MapElement where
  show Obst = "#"
  show (Empty UnVisited _) = "."
  show (Empty Visited dir) = show $ length dir
  show (Guard a) = show a

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

moveGuard2 :: [Orientation] -> Array2d MapElement -> Maybe (Array2d MapElement)
moveGuard2 carry_dir cur = do
  nextPlace <- moveOrTurn carry_dir cur
  undefined



runGuard :: Array2d MapElement -> Array2d MapElement
runGuard = execState $ moveGuard []

type GuardStatus = ((Int,Int), Orientation)
curGuard :: Array2d MapElement -> GuardStatus
curGuard cur =
  let elements = elems cur
      guardIdx = indices cur !! fromJust ( elemIndex (Guard North) elements)
      orient = orientation $ cur ! guardIdx
   in (guardIdx, orient)

checkAhead :: Array2d MapElement -> Maybe MapElement
checkAhead cur = do
  new <- ahead cur
  return $ cur ! new

checkAhead' :: Array2d MapElement -> GuardStatus -> Maybe MapElement
checkAhead' cur gs = do
  new <- ahead' cur gs
  return $ cur ! new

ahead' :: Array2d MapElement -> GuardStatus -> Maybe (Int, Int)
ahead' cur ((h,w),orient) =
  let n = case orient of
        North -> (h - 1, w)
        East -> (h, w + 1)
        South -> (h + 1, w)
        West -> (h, w - 1)
  in if inRange (bounds cur) n then return n else Nothing

ahead :: Array2d MapElement -> Maybe (Int, Int)
ahead cur =
  let guardStatus = curGuard cur
  in ahead' cur guardStatus
      -- n = case orient of
      --   North -> (h - 1, w)
      --   East -> (h, w + 1)
      --   South -> (h + 1, w)
      --   West -> (h, w - 1)
      -- in if inRange (bounds cur) n then return n else Nothing

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
    Guard _ -> undefined

data LoopOrExit = Exit | Loop

maybeToExit :: Maybe a -> Either LoopOrExit a
maybeToExit Nothing = Left Exit
maybeToExit (Just a) = Right a

moveOrTurn2 :: [Orientation] -> Array2d MapElement -> Either LoopOrExit (Array2d MapElement, [Orientation])
moveOrTurn2 carry_dir cur = do
  let guard_status = curGuard cur
  a <- maybeToExit $ checkAhead' cur guard_status
  nextIdx <- maybeToExit $ ahead' cur guard_status
  case a of
    Obst ->
      let (idx, o) = guard_status
      in return $ (cur // [(idx, Guard $ next o)], o:carry_dir)
    Empty _ prev_o ->
      let (idx, o) = guard_status
      in
        if
          o `elem` prev_o
        then trace ((showMap cur)) $ Left Loop
        -- then Left Loop
        else return $ (cur // [(idx, updateEmpty carry_dir o), (nextIdx, Guard o)], prev_o)
    Guard _ -> undefined

checkLoop ::  Array2d MapElement -> Bool
-- checkLoop x =   moveGuardLoop3 $ TrackGuard {curMap = x, nIter = 0, carryState = []}
checkLoop = moveGuardLoop4 [] 0

tryVisited ::Array2d MapElement -> [Bool]
tryVisited cur = let
    -- start = fst $ fromJust $ find (\i -> snd i== Guard North ) $ assocs cur
    start = fst $ fromJust $ find ((==Guard North).snd) $ assocs cur
    -- start = fromJust $ elemIndex (Guard North) (elems cur)
    comp = runGuard cur
    -- vals = findIndices  isVisited $ elems comp
    idxs = filter (/=start) $ map fst $ filter (isVisited . snd) $ assocs comp
    -- idxs = indices cur
    initMap = [cur // [(idx, Obst)] | idx <- idxs]
    in parMap rdeepseq checkLoop initMap

tryAll :: Array2d MapElement -> [Bool]
tryAll cur = let
  vals = findIndices isEmpty  $ elems cur
  idxs = indices cur
  initMap = [cur // [(idxs !! i, Obst)] | i <- vals ]
  in parMap rdeepseq checkLoop initMap

countLoops :: [Bool] -> Int
countLoops = length . filter id

data TrackGuard = TrackGuard {curMap :: (Array2d MapElement), nIter :: Int, carryState:: [Orientation]}

moveGuardLoop4 :: [Orientation] -> Int -> Array2d MapElement -> Bool
moveGuardLoop4 carry_dir n cur = --if n >= max_iter then True else
  let
    next_map = moveOrTurn2 carry_dir cur
  in
    case next_map of
      Left Exit -> False
      Left Loop -> True
      Right (m, s) -> moveGuardLoop4 s (succ n) m

moveGuardLoop3 :: TrackGuard -> Bool
moveGuardLoop3 prev_state = if (nIter prev_state) >= max_iter
    then
      True
    else
      let
        next_pos = checkAhead' (curMap prev_state) ((h,w),guard_dir)
        next_map = moveOrTurn (carryState prev_state) (curMap prev_state)
        ((h,w), guard_dir) = curGuard (curMap prev_state)
      in
        case next_pos of
          Nothing -> False
          Just el -> case el of
            Obst -> moveGuardLoop3 $ TrackGuard {curMap = (fromJust next_map),  nIter=(nIter prev_state), carryState=[]}
            Empty UnVisited _ -> moveGuardLoop3 $ TrackGuard {curMap = (fromJust next_map), nIter=(succ $ nIter prev_state), carryState=[]}
            Empty Visited dir ->
              if guard_dir `elem` dir
                then
                  True
                else
                  moveGuardLoop3 $ TrackGuard {curMap=(fromJust next_map), nIter=(succ $ nIter prev_state), carryState=(carryState prev_state)}
            Guard _ -> undefined



moveGuardLoop2 :: (Monad m) => [Orientation] -> StateT (Int, Array2d MapElement) m Bool
moveGuardLoop2 carry_state = do
  (c, cur) <- get
  let a =  checkAhead cur
  let nextPlace = moveOrTurn carry_state cur
  let (_, guard_dir) = curGuard cur
  case a of
    Nothing -> return False
    Just e -> case e of
      Obst ->
        if c >= 2500 then return True else do
            (s, m) <- return $ runState (moveGuardLoop2 []) (c, fromJust nextPlace)
            put m
            return s
      Empty UnVisited _ -> do
        (s, m) <- return $ runState (moveGuardLoop2 []) (c+1,fromJust nextPlace)
        put m
        return s
      Empty Visited dir ->
        if (guard_dir `elem` dir) ||  c >=10000 then  return True
        -- if (guard_dir `elem`  dir) || (length dir>4) || c >=50000 then trace (show cur) $ return True
        -- if (guard_dir `elem` dir) || (length dir>4) || c >=25000 then trace (concatMap show dir++"\n"++showMap cur) $ return True
        -- if guard_dir `elem` dir then return True
      -- Empty _ dir ->
      --   if sameDirection go dir then (trace $ show $ elems cur) $ return True
        else do
          (s, m) <- return $ runState (moveGuardLoop2 dir) (c+1,fromJust nextPlace)
          put m
          return s
      _ -> undefined

-- sameDirection :: Orientation -> EmptyState -> Bool
-- sameDirection North North' = True
-- sameDirection South South' = True
-- sameDirection East East' = True
-- sameDirection West West' = True
-- sameDirection _ _ = False
