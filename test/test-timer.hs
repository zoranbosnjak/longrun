
module Main where

import System.Log.Logger hiding (logM)

import Control.Concurrent.Longrun
import Testrun

main :: IO ()
main = runScenario
    [ ("tim1", tim1)
    , ("tim2", tim2)
    , ("timLoop", timLoop)
    , ("timExpire", timExpire)
    , ("timException", timException)
    , ("timRestart", timRestart)
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
        nop
        --sleep 0.001
    
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
    
-- | Terminate parent.
timException :: Process ()
timException = do
    logM INFO "hello"
    t <- newTimer "timer" 1 $ do
        logM INFO "expired"
        die "terminate"
    _ <- restartTimer t
    rest

-- | Periodic timer restart, check memory.
timRestart :: Process ()
timRestart = do
    t <- newTimer "timer" 0.001 $ do
        logM INFO "tick"
        --nop
    forever $ do
        _ <- restartTimer t
        sleep 0.002

