module Main (main) where
import Control.Monad.Trans.Except ( runExceptT )
import System.IO.Error ( isDoesNotExistError, isUserError )
import Control.Exception ( throw )
import Test.HUnit
    ( assertBool,
      assertEqual,
      runTestTTAndExit,
      Test(TestCase, TestList, TestLabel) )
import Utils ( readFileFromArg )


mockArgsTrue :: IO [String]
mockArgsTrue = return ["./test/test_file.dat"]
mockArgsIncorrect :: IO [String]
mockArgsIncorrect = return ["./test/non-existant-file"]

mockArgsEmpty :: IO [String]
mockArgsEmpty = return []

noArgMsg :: IO String
noArgMsg = return "No arguments given"

noFileMsg :: IO String
noFileMsg = return $ "File does not exist" 

-- Example usage
readFileFromArgTest :: IO [String] -> IO String
readFileFromArgTest args = do
    fallable_content <- runExceptT $ readFileFromArg args
    case fallable_content of
        Right content -> return content
        Left err | isUserError err -> noArgMsg
        Left err | isDoesNotExistError err -> noFileMsg
        Left err -> throw err

tests = TestList[
    TestLabel "Empty" (TestCase (do
        msg <- (noArgMsg) 
        output <- (readFileFromArgTest mockArgsEmpty)
        assertEqual "testing no arguments" msg output
        )),
    TestLabel "No file" (TestCase (do
        msg <- (noFileMsg) 
        output <- (readFileFromArgTest mockArgsIncorrect)
        assertEqual "testing no arguments" msg output
        )),
    TestLabel "True" (TestCase (do
        content <- readFileFromArgTest mockArgsTrue
        assertBool "test with args" (read content :: Bool)
        ))
    ]


main :: IO ()
main = runTestTTAndExit tests
