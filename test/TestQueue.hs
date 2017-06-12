module TestQueue (
    testQueue
) where

import System.Log.Logger (Priority(INFO))
import Test.Framework (Test, testGroup)
import Utils (testLogsOfMatch)

import Control.Concurrent.Longrun

testQueue :: Test
testQueue = testGroup "testQueue"
    [ testBoundedQueue
    ]

-- | Bounded queue
procBoundedQueue :: Process ()
procBoundedQueue = do
    (writeEnd, readEnd) <- newQueue1
    writeQueueBlocking writeEnd "a"
    readQueueBlocking readEnd >>= logM INFO
    writeQueueBlocking writeEnd "c"
    t <- spawnTask $ do
        writeQueueBlocking writeEnd "b"
    rv <- waitCatch t
    logM INFO $ show rv
    logM INFO "done"

testBoundedQueue :: Test
testBoundedQueue = testLogsOfMatch "bounded queue" DEBUG procBoundedQueue
    [ (INFO, "a")
    , (INFO, "Left thread blocked indefinitely in an STM transaction")
    , (INFO, "done")]
