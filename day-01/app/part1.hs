import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import System.Environment
import System.IO

main :: IO ()
main = do
  maybe_path <- runMaybeT getFileName
  case maybe_path of
    Just path -> do
      common <- liftIO (getData path) >>= srtLists
      part1 <- absDiff common >>= return . sum
      part2 <- uncurry mapSimilarity common >>= return . sum
      liftIO $ putStrLn "The answer to part 1 is:"
      liftIO $ putStrLn $ show part1
      liftIO $ putStrLn "The answer to part 2 is:"
      liftIO $ putStrLn $ show part2
    Nothing -> do
      putStrLn "Please provide a path"

getFileName :: MaybeT IO String
getFileName = do
  args <- liftIO getArgs
  fileName <- MaybeT $ return $ listToMaybe args
  return fileName

getData :: FilePath -> IO [(Int, Int)]
getData path = openFile path ReadMode >>= hGetContents >>= return . parse
  where
    parse = (map take2) . (map (map (read :: String -> Int))) . (map words) . lines
    take2 (a : b : []) = (a, b)

srtLists :: (Ord a, Monad m) => [(a, a)] -> m ([a], [a])
srtLists = return . (mapT sort) . unzip
  where
    mapT f (a, b) = ((f a), (f b))

absDiff :: (Num a, Monad m) => ([a], [a]) -> m [a]
absDiff = return . (map (abs . uncurry (-))) . uncurry zip

similarity :: Int -> [Int] -> Int
similarity a b = (* a) . length $ filter (== a) b

mapSimilarity :: (Monad m) => [Int] -> [Int] -> m [Int]
mapSimilarity a b = return $ map (\a' -> similarity a' b) a
