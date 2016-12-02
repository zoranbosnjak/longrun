import Control.Concurrent.STM
import Control.Monad hiding (forever)
import Control.Monad.Trans.Reader
import Data.Set as Set
import System.Log.Logger hiding (logM)

import Control.Concurrent.Longrun
import Testrun

main :: IO ()
main = runScenario
    [ ("task1", task False)
    , ("task2", task True)
    , ("taskRepeat", taskRepeat)
    , ("proc1", proc1)
    , ("procproc", procproc)
    , ("autoStop1", autoStop1)
    , ("autoStop2", autoStop2)
    , ("term", term)
    , ("inLoop1", inLoop1)
    , ("inLoop2", inLoop2)
    ]

-- | Regular task with failure or without
task :: Bool -> Process ()
task failFlag = do
    logM INFO $ "startup: " ++ show failFlag
    t <- spawnTask "test" $ do
        logM INFO "task start"
        sleep 0.1
        when failFlag $ fail "fail request"
        return "OK"
    rv <- waitCatch t
    logM INFO $ "result: " ++ show rv

-- | Repeat running task, observe memory
taskRepeat :: Process ()
taskRepeat = forM_ [(1::Integer)..] $ \cnt -> do
    t <- spawnTask ("iterration " ++ show cnt) $ do
        sleep 0.001
        return cnt
    rv <- waitCatch t
    logM INFO $ show rv

-- | Regular long running process.
proc1 :: Process ()
proc1 = do
    logM INFO "startup"
    _ticker <- spawnProcess "tickerProcess" nop $ forever $ do
        logM INFO "tick"
        sleep 1.0
    rest

-- | Process in process.
procproc :: Process ()
procproc = forever $ do
    logM INFO "startup"
    let f delta = do
            logM INFO $ show delta
            sleep delta
        
    p1 <- spawnProcess "p1" nop $ do
        _p1a <- spawnProcess "a" nop $ forever (f 1.1)
        _p1b <- spawnProcess "b" nop $ forever (f 1.11)
        rest
    p2 <- spawnProcess "p2" nop (forever (f 1.2))

    sleep 3
    logM INFO "stop p1"
    stop_ p1
    sleep 2
    logM INFO "stop p2"
    stop_ p2

-- | Implicit childs stop (ver1)
autoStop1 :: Process ()
autoStop1 = do
    logM INFO "start"
    t <- spawnTask "task" $ do
        _ <- spawnTask "subtask" $ do
            sleep 1
            runIO $ putStrLn "this should not be displayed"
        sleep 1
        runIO $ putStrLn "this should also not be displayed"
    c1 <- asks procChilds >>= runIO . atomically . readTVar
    logM INFO $ "c1: " ++ show (Set.size c1)
    sleep 0.5
    stop_ t
    c2 <- asks procChilds >>= runIO . atomically . readTVar
    logM INFO $ "c2: " ++ show (Set.size c2)
    sleep 1
    logM INFO "done"

-- | Implicit childs stop (ver2)
autoStop2 :: Process ()
autoStop2 = do
    logM INFO "start"
    t <- spawnTask "task" $ do
        _ <- spawnTask "subtask" $ do
            sleep 1
            runIO $ putStrLn "this should not be displayed"
        sleep 0.5
        _ <- fail "oops"
        rest
    sleep 1
    c1 <- asks procChilds >>= runIO . atomically . readTVar
    logM INFO $ "c1: " ++ show (Set.size c1)

    _ <- waitCatch t
    c2 <- asks procChilds >>= runIO . atomically . readTVar
    logM INFO $ "c2: " ++ show (Set.size c2)

-- | Termination
term :: Process ()
term = do
    _ <- spawnProcess "proc" (logM ERROR "termination") $ do
        logM INFO "will stop now"
    rest

inLoop1 :: Process ()
inLoop1 = do
    t <- spawnTask "tsk" $ do
        sleep 0.2
        nop
    forever $ do
        sleep 0.00001
        stop_ t

inLoop2 :: Process ()
inLoop2 = forever $ do
    t <- spawnTask "tsk" $ do
        sleep 0.2
        nop
    sleep 0.00001
    stop_ t
    
