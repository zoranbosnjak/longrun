
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
    [ ("tim1", tim1)
    , ("tim2", tim2)
    , ("timLoop", timLoop)
    , ("timExpire", timExpire)
    ]

-- | Basic timer
tim1 :: Process ()
tim1 = do
    t <- newTimer "timer" 1 $ do
        logM INFO "done"
    _ <- restartTimer t
    sleep 1.1
    logM INFO "bye"

-- | Stop/restart
tim2 :: Process ()
tim2 = do
    t <- newTimer "timer" 1 $ do
        logM INFO "done"
    _ <- restartTimer t
    sleep 0.5
    _ <- stopTimer t
    _ <- restartTimer t
    rest

-- | Check memory.
timLoop :: Process ()
timLoop = do
    t <- newTimer "timer" 0.1 $ do
        _sub <- spawnProcess "task" $ forever $ do
            logM INFO "task"
            sleep 0.1
        sleep 0.3
        logM INFO "done"

    forever $ do
        _ <- restartTimer t
        sleep 0.5
    
-- | Expire timer.
timExpire :: Process ()
timExpire = do
    t <- newTimer "timer" 2 $ do
        logM INFO "tick"
    logM INFO "start"
    _ <- expireTimer t
    _ <- restartTimer t
    sleep 0.1
    _ <- expireTimer t
    logM INFO "stop"
    
