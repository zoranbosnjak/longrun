
module Main where

import Control.Monad hiding (forever)
import System.Environment (getArgs)
import System.Log.Logger hiding (logM)
import System.Log.Handler.Simple
import System.IO

import Control.Concurrent.Longrun

main :: IO ()
main = do
    args <- getArgs

    -- setup console logging
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG . removeHandler)
    hConsole <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (addHandler hConsole)

    -- run all tests from command line
    forM_ args $ \arg -> do
        putStrLn ""
        putStrLn $ "running: " ++ show arg
        let mFunc = lookup arg scenarios
        case mFunc of
            Nothing -> error $ show arg ++ " not found."
            Just func -> runApp func

scenarios :: [(String, Process ())]
scenarios =
    [ ("var1", var1)
    , ("var2", var2)
    ]

-- | Basic variable
var1 :: Process ()
var1 = do
    var <- newVar "var1" "content"
    val1 <- getVar' var
    logM INFO $ show val1
    setVar' var "new content"
    val2 <- getVar' var
    logM INFO $ show val2

-- | Modify variable
var2 :: Process ()
var2 = do
    var <- newVar "var" (1::Int)
    (oldVal,newVal) <- modifyVar var (+1)
    val <- getVar' var
    logM INFO $ show oldVal
    logM INFO $ show newVal
    logM INFO $ show val

