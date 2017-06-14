
{-# LANGUAGE FlexibleContexts #-}

module TestWait (
    testWait
) where

import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Utils (assertConstantMemory, testLogsOfMatch, runAppWithoutLogging)

import qualified Control.Concurrent.Longrun as Longrun

testWait :: Test
testWait = testGroup "Test task sleep"
    [ testTimeout
    , testMem
    , testWriteQueueTimeout
    ]


procTimeout :: Longrun.Process ()
procTimeout = do
    Longrun.logM Longrun.INFO "start"
    (writeEnd, readEnd) <- Longrun.newQueue1
    _ <- Longrun.spawnTask $ do
        Longrun.sleep 0.1
        Longrun.logM Longrun.INFO "awoke"
        Longrun.writeQueueBlocking writeEnd "hello"
    Longrun.logM Longrun.INFO "spawned"
    msg <- Longrun.readQueueTimeout 0.2 readEnd
    Longrun.logM Longrun.INFO $ show msg

testTimeout :: Test
testTimeout = testLogsOfMatch "timeout" Longrun.DEBUG procTimeout
    [ (Longrun.INFO,"start")
    , (Longrun.INFO, "spawned")
    , (Longrun.INFO, "awoke")
    , (Longrun.INFO, "Just \"hello\"")
    ]

procMem :: Longrun.Process ()
procMem = do

    -- case false
    do
        (writeEnd, readEnd) <- Longrun.newQueue1
        t <- Longrun.spawnTask $ do
            Longrun.sleep 0.004
            Longrun.writeQueueBlocking writeEnd "hello"
        msg <- Longrun.readQueueTimeout 0.003 readEnd
        _ <- Longrun.stop t
        Longrun.logM Longrun.INFO $ show msg

    -- case true
    do
        (writeEnd, readEnd) <- Longrun.newQueue1
        t <- Longrun.spawnTask $ do
            Longrun.sleep 0.002
            Longrun.writeQueueBlocking writeEnd "hello"
        msg <- Longrun.readQueueTimeout 0.003 readEnd
        _ <- Longrun.stop t
        Longrun.logM Longrun.INFO $ show msg

testMem :: Test
testMem = buildTest $
    fmap (testCase "queue timeout memory leak") $
        runAppWithoutLogging $ assertConstantMemory 100 1.2 procMem


procWriteQueueTimeout :: Longrun.Process [Bool]
procWriteQueueTimeout = do
    (writeEnd, _) <- Longrun.newQueue 2
    sequence $ replicate 4 $ Longrun.writeQueueTimeout 0.01 writeEnd ()

testWriteQueueTimeout :: Test
testWriteQueueTimeout = buildTest $
    fmap (testCase "write to queue with timeout") $ do
        pattern <- runAppWithoutLogging procWriteQueueTimeout
        return $ assertEqual "write" [True, True, False, False] pattern
