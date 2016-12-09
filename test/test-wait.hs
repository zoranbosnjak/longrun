import System.Log.Logger hiding (logM)

import Control.Concurrent.Longrun
import Testrun

main :: IO ()
main = runScenario
    [ ("wait", testWait)
    , ("mem", testMem)
    ]

testWait :: Process ()
testWait = do
    logM INFO "start"
    q <- newQueue1 "test"
    _ <- spawnTask "send" $ do
        sleep 0.5
        writeQueue' q "hello"
    msg <- readQueueTimeout (ReadEnd q) 0.6
    logM INFO $ show msg

testMem :: Process ()
testMem = forever $ do

    -- case false
    do
        q <- newQueue1 "q"
        t <- spawnTask "send" $ do
            sleep 0.004
            writeQueue' q "hello"
        msg <- readQueueTimeout (ReadEnd q) 0.003
        _ <- stop t
        logM INFO $ show msg

    -- case true
    do
        q <- newQueue1 "q"
        t <- spawnTask "send" $ do
            sleep 0.002
            writeQueue' q "hello"
        msg <- readQueueTimeout (ReadEnd q) 0.003
        _ <- stop t
        logM INFO $ show msg

