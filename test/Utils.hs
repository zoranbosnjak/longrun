module Utils where

import Control.Concurrent.Longrun (Process, runApp)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import InMemoryLogger (inMemoryLogger, evacuateLogger)
import System.Log (LogRecord)
import Test.Framework (Test)
import Test.Framework (buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), Assertion)
import qualified System.Log.Logger as Logger


resetLogger :: IO ()
resetLogger = do
    Logger.updateGlobalLogger Logger.rootLoggerName
        (Logger.setLevel Logger.DEBUG . Logger.removeHandler)

captureLogs :: IO a -> IO ([LogRecord], a)
captureLogs computation = do
    resetLogger
    handler <- inMemoryLogger
    Logger.updateGlobalLogger Logger.rootLoggerName (Logger.addHandler handler)
    a <- computation
    logs <- evacuateLogger handler
    resetLogger
    return (logs, a)

assertLogsWithoutTimeEqual :: [LogRecord] -> [LogRecord] -> Assertion
assertLogsWithoutTimeEqual value expected =
    expected @=? fmap f value where
       f (priority, msg) = (priority, msg') where
           msg' = dropWhileEnd isThrowaway $ dropWhileEnd (/= '@') msg
           isThrowaway c = isSpace c || (c == '@')

testLogsOfMatch :: String -> Process () -> [LogRecord] -> Test
testLogsOfMatch name proc expected = buildTest $ do
    (logs, _) <- captureLogs $ runApp proc
    return $ testCase name $
        assertLogsWithoutTimeEqual logs expected
