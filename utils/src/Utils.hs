module Utils (getFileName, readFileFromArg) where

import Control.Exception (try)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (..) )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Maybe (listToMaybe)

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
