module Main where
import Utils
import Text.Parsec
import Text.Parsec.String
import Data.List (find)
import Data.Maybe
import Data.Either (fromRight)
import Debug.Trace

main :: IO ()
main = Utils.runTask run

run :: AOC
run content = return (show $  (Main.readFile content) >>= part1 , Just $ show $ (Main.readFile content) >>= part2)

data Comp = Comp {answer :: Int, inputs :: [Int]} deriving (Show)

parseTask = undefined

parseAns :: Parser Int
parseAns = (read :: String -> Int) <$> manyTill digit (char ':') <* char ' ' 
parseInputs :: Parser [Int]
parseInputs = map read <$> manyTill (many1 digit <* optional ( char ' ')) newline

parseLine :: Parser Comp
parseLine = Comp <$> parseAns <*> parseInputs

parseFile :: Parser [Comp]
parseFile = many1 parseLine <* optional newline <* eof

readFile = parse parseFile "Parsing File" 

type Func a = a -> a -> a

myFoldl :: (Num a) => [Func a] -> a -> [a] -> a
myFoldl _ z [] = z
myFoldl (f:_) z [x] = f z x
myFoldl (f:fs) z (x:xs) = let z' = f z x in (myFoldl fs z' xs)

test :: [Int -> Int -> Int]-> Comp  -> Maybe Int
test ops x = let
    len = length $ inputs x
    perms = map ([(+)]++) (sequence $ replicate (len-1) ops)
    cases = [myFoldl p 0 (inputs x) | p <- perms]
    in
    find (== answer x) cases

part1 :: (Monad m) => [Comp] -> m Int
part1 a = let 
    addMul = [(+),(*)]  
    t = mapMaybe (test addMul) a 
    in return $ sum t

concatNum :: (Show a) => a -> a -> Int
concatNum a b = (read :: String -> Int) $ (show a) ++ (show b)

part2 :: (Monad m) => [Comp] -> m Int
part2 a = let
    addMulCat = [(+),(*),concatNum]
    t = mapMaybe (test addMulCat) a
    in return $ sum t


