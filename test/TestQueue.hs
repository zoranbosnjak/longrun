module TestQueue (
    testQueue
) where

import System.Log.Logger (Priority(INFO))
import Test.Framework (Test, testGroup)
import Utils (testLogsOfMatch)

import Control.Concurrent.Longrun

testQueue :: Test
testQueue = testGroup "testQueue"
    [ testUnboundedQueue
    , testBoundedQueue
    ]

-- | Unbounded queue
procUnboundedQueue :: Process ()
procUnboundedQueue = do
    q <- newQueue Nothing "q"
    writeQueue' q "a"
    writeQueue' q "b"
    readQueue' q >>= logM INFO . show
    _ <- spawnTask "reader" $ do
        readQueue' q >>= logM INFO . show
        readQueue' q >>= logM INFO . show
        logM INFO "this shall not be displayed"
    sleep 1
    logM INFO "bye"

testUnboundedQueue :: Test
testUnboundedQueue = testLogsOfMatch "unbounded queue" INFO procUnboundedQueue
    [ (INFO, "\"a\"")
    , (INFO, "\"b\"")
    , (INFO, "bye")
    ]


-- | Bounded queue
procBoundedQueue :: Process ()
procBoundedQueue = do
    q <- newQueue1 "q"
    writeQueue' q "a"
    readQueue' q >>= logM INFO . show
    writeQueue' q "c"
    t <- spawnTask "writer" $ do
        writeQueue' q "b"
    rv <- waitCatch t
    logM INFO $ show rv
    logM INFO "done"

testBoundedQueue :: Test
testBoundedQueue = testLogsOfMatch "bounded queue" DEBUG procBoundedQueue
    [ (DEBUG, "newQueue (bounded 1)")
    , (DEBUG, "writeQueue, value: \"a\"")
    , (DEBUG, "readQueue, value: \"a\"")
    , (INFO, "\"a\"")
    , (DEBUG, "writeQueue, value: \"c\"")
    , (DEBUG, "spawnTask")
    , (DEBUG, "addChild")
    , (DEBUG, "waitCatch")
    , (DEBUG, "removeChild")
    , (INFO, "Left thread blocked indefinitely in an STM transaction")
    , (INFO, "done")]
