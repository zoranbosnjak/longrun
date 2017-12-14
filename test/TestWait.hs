
{-# LANGUAGE FlexibleContexts #-}

module TestWait (
    testWait
) where

import Data.Maybe (isJust)
import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Utils (assertConstantMemory, testLogsOfMatch, runAppWithoutLogging)

import Control.Concurrent.Longrun as Longrun

testWait :: Test
testWait = testGroup "Test task sleep"
    [ testTimeout
    , testMem
    , testWriteQueueTimeout
    ]


procTimeout :: Process ()
procTimeout = do
    logM INFO "start"
    (writeEnd, readEnd) <- newBoundedQueue1
    _ <- spawnTask $ do
        sleep 0.1
        logM INFO "awoke"
        writeQueueBlocking writeEnd "hello"
    logM INFO "spawned"
    msg <- withTimeout 0.2 $ readQueueBlocking readEnd
    logM INFO $ show msg

testTimeout :: Test
testTimeout = testLogsOfMatch "timeout" DEBUG procTimeout
    [ (INFO,"start")
    , (INFO, "spawned")
    , (INFO, "awoke")
    , (INFO, "Just \"hello\"")
    ]

procMem :: Process ()
procMem = do

    -- case false
    do
        (writeEnd, readEnd) <- newBoundedQueue1
        t <- spawnTask $ do
            sleep 0.004
            writeQueueBlocking writeEnd "hello"
        msg <- withTimeout 0.003 $ readQueueBlocking readEnd
        _ <- stop t
        logM INFO $ show msg

    -- case true
    do
        (writeEnd, readEnd) <- newBoundedQueue1
        t <- spawnTask $ do
            sleep 0.002
            writeQueueBlocking writeEnd "hello"
        msg <- withTimeout 0.003 $ readQueueBlocking readEnd
        _ <- stop t
        logM INFO $ show msg

testMem :: Test
testMem = buildTest $
    fmap (testCase "queue timeout memory leak") $
        runAppWithoutLogging $ assertConstantMemory 100 1.2 procMem


procWriteQueueTimeout :: Process [Bool]
procWriteQueueTimeout = do
    (writeEnd, _) <- newBoundedQueue 2
    sequence $ replicate 4 $ fmap isJust $ withTimeout 0.01 $
        writeQueueBlocking writeEnd ()

testWriteQueueTimeout :: Test
testWriteQueueTimeout = buildTest $
    fmap (testCase "write to queue with timeout") $ do
        pattern <- runAppWithoutLogging procWriteQueueTimeout
        return $ assertEqual "write" [True, True, False, False] pattern

