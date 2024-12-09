module Main where
import Utils
import Text.Parsec
import Text.Parsec.String
main :: IO ()
main = Utils.runTask run

run :: AOC
run content = return (show $ Main.readFile content , Nothing)

data Comp = Comp {answer :: Int, inputs :: [Int]} deriving (Show)

parseTask = undefined

parseAns :: Parser Int
parseAns = (read :: String -> Int) <$> manyTill digit (char ':') <* char ' ' 
parseInputs :: Parser [Int]
parseInputs = map read <$> manyTill (many1 digit <* char ' ') newline

parseLine :: Parser Comp
parseLine = Comp <$> parseAns <*> parseInputs

parseFile :: Parser [Comp]
parseFile = many1 parseLine <* eof

readFile = parse parseFile "Parsing File" 
