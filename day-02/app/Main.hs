module Main where
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Environment
import System.IO
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
    maybe_path <- runMaybeT getFileName
    case maybe_path of
        Nothing -> do
            putStrLn "Please provide a path"
        Just path -> do
            d <- getData path
            deriv <- return $ map finiteDifference d
            countSafe <- return $ length $ filter isSafe deriv
            countSafeDamped <- return $ length $ filter id (map tryDamped d)
            putStrLn "The answer to part 1 is:"
            putStrLn $ show countSafe
            putStrLn "The answer to part 2 is:"
            putStrLn $  show countSafeDamped

getFileName:: MaybeT IO String
getFileName = do
    args <- liftIO getArgs
    filename <- MaybeT $ return $ listToMaybe args
    return filename

getData :: FilePath -> IO [[Int]]
getData path = openFile path ReadMode >>= hGetContents >>= return . parse
    where parse = (map (map (read :: String -> Int))) . (map words) . lines

finiteDifference :: (Num a) => [a] -> [a]
finiteDifference x = [ (x !! i) - (x !! (i-1)) | i <- [1..len]]
    where len = (length x) -1

isMonotonic :: (Num a, Ord a) => [a] -> Bool
isMonotonic x = (all (> 0) x) || (all (< 0) x)

isSmall :: (Num a, Ord a) => [a] -> Bool
isSmall x= all (<=3) $ map abs x

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe x = isMonotonic x && isSmall x

tryDamped :: (Num a, Ord a) => [a] -> Bool
tryDamped x = any isSafe $ map finiteDifference [subset i x | i <- [0..len-1]]
    where len = length x
          subset i x = let (h, t) = splitAt i x in h ++ drop 1 t
