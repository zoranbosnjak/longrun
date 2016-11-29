module Testrun where

import System.Environment (getArgs)
import System.Log.Logger hiding (logM)
import System.Log.Handler.Simple
import System.IO

import Control.Concurrent.Longrun

runScenario :: [(String, Process a)] -> IO a
runScenario scenarios = do

    -- setup console logging
    updateGlobalLogger rootLoggerName 
        (System.Log.Logger.setLevel DEBUG . removeHandler)
    hConsole <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (addHandler hConsole)

    -- run selected test
    (arg:_) <- getArgs
    let mFunc = lookup arg scenarios
    case mFunc of
        Nothing -> error $ show arg ++ " not found."
        Just func -> runApp func

