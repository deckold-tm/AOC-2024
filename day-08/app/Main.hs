module Main where
import Utils
import Data.Array
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = runTask day08

day08 :: AOC
day08 = undefined


type Bounds = (Int,Int)
type Array2d = Array Bounds


data MapElement = C Char| E

instance Eq MapElement where
    (C a) == (C b) = a == b
    E == E = True
    _ == _ = False

parseEmpty :: Parser MapElement
parseEmpty = E <$ char '.'

parseAntenna :: Parser MapElement
parseAntenna = C <$> alphaNum

parseLine :: Parser [MapElement]
parseLine = manyTill (parseEmpty <|> parseAntenna) newline

parseWholeFile :: Parser [[MapElement]]
parseWholeFile = manyTill parseLine eof

stringToArray :: [[MapElement]] -> Array2d MapElement
stringToArray s = listArray b $ concat s
    where b = let xdim = length $ head s
                  ydim = length s in
                  ((1,1),(ydim,xdim))

isAntenna :: MapElement -> Bool
isAntenna x = case x of
    C _ -> True
    _ -> False


listAntenna :: Array2d MapElement -> [(Bounds, MapElement)]
listAntenna = filter (isAntenna.snd) . assocs 

filterAntennas :: (Bounds, MapElement) -> [(Bounds, MapElement)] -> [(Bounds, MapElement)]
filterAntennas (b, cur) m = filter ((/=b).fst) $ filter ((==cur).snd) m

findNodes :: (Bounds, MapElement) -> [(Bounds, MapElement)] -> [Bounds]
findNodes (b, e) m = let others = filterAntennas (b,e)
    in map (dist (b, e)) others

dist :: Bounds -> Bounds -> Bounds
dist (a1,b1) (a2,b2) = (a2-a1, b2-b1)


