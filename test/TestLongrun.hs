module TestLongrun (
    testLongrun
) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Random (newStdGen, randoms)
import Test.Framework (Test, buildTest, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Utils (assertConstantMemory, runAppWithoutLogging)

import Control.Concurrent.Longrun

testLongrun :: Test
testLongrun = testGroup "test Longrun"
    [ testLong
    ]

testLong :: Test
testLong = buildTest $ fmap (testCase "long queue pipe") $
    runAppWithoutLogging $ do
        stdGen <- liftIO newStdGen
        (writeEnd, readEnd) <- newBoundedQueue1

        _src <- spawnProcess $
            forM_ (randoms stdGen :: [Int]) $ \x -> do
                writeQueueBlocking writeEnd x
                sleep 0.001

        _sink <- spawnProcess $ forever $ do
            _ <- readQueueBlocking readEnd
            sleep 0.001

        assertion <- assertConstantMemory 100 1.2 $ do
            sleep 0.001

        stop_ _sink
        stop_ _src
        return assertion
