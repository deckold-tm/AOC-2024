module Utils (getFileName, readFileFromArg, runTask, AOC) where

import Control.Exception (try, throw)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.IO.Error ( isDoesNotExistError, isUserError )

getFileName :: IO [String] -> ExceptT IOError IO FilePath
getFileName args = do
  args' <- liftIO $ args
  fallable_arg <- maybeToExceptT noFileGiven $ MaybeT . return $ listToMaybe args'
  return fallable_arg

noFileGiven :: IOError
noFileGiven = userError "No file path provided"

readFileFromArg :: IO [String] -> ExceptT IOError IO String
readFileFromArg args = do
  path <- getFileName args
  content <- ExceptT $ do try $ readFile path
  return content

type AOC = String -> IO (String, Maybe String)

runTask :: AOC -> IO ()
runTask func = do
  content <- runExceptT $ readFileFromArg getArgs
  case content of
    Left err | isUserError err -> undefined
    Left err | isDoesNotExistError err -> undefined
    Left err -> throw err
    Right c -> do
      (ans1, ans2) <- func c
      putStrLn "The answer to part 1 is:"
      putStrLn ans1
      case ans2 of
        Just ans-> putStrLn $ "The answer to part 2 is:\n" ++ ans
        Nothing -> return ()
