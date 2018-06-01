{-# LANGUAGE CPP #-}

module Utils where

import Control.Exception (SomeException, catch)
import Control.Concurrent.Longrun (noLogger, Process, runAppWithConfig, AppConfig (AppConfig))
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import InMemoryLogger (inMemoryLogger, evacuateLogger, logInMemory)
import System.Environment (lookupEnv)
import System.Log (LogRecord)
import Test.Framework (Test)
import Test.Framework (buildTest)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), Assertion, assertString)
import qualified GHC.Stats as Stats
import qualified System.Log.Logger as Logger
import qualified System.Mem as Mem


runAppWithoutLogging :: Process a -> IO a
runAppWithoutLogging = runAppWithConfig $ AppConfig noLogger

captureLogs :: Logger.Priority -> Process a -> IO ([LogRecord], Either SomeException a)
captureLogs priority app = do
    logger <- inMemoryLogger
    let cfg = AppConfig $ logInMemory logger
        computation = runAppWithConfig cfg app
    a <- catch (Right <$> computation) (return . Left)
    logs <- filter ((>= priority) . fst) <$> evacuateLogger logger
    return (logs, a)

testLogsOfMatch :: String -> Logger.Priority -> Process () -> [LogRecord] -> Test
testLogsOfMatch name priority proc expected = buildTest $ do
    (logs, _) <- captureLogs priority proc
    return $ testCase name $ expected @=? logs

getUsedMemory :: IO Int64
getUsedMemory = do
    Mem.performGC  -- stats are only refreshed after a GC cycle
#if __GLASGOW_HASKELL__ < 802
    Stats.currentBytesUsed <$> Stats.getGCStats
#else
    fromIntegral . Stats.gcdetails_live_bytes . Stats.gc <$> Stats.getRTSStats
#endif

assertConstantMemory :: MonadIO m => Int64 -> Double -> m a -> m Assertion
assertConstantMemory baseIterations maxRatio block = do
    multiplier <- liftIO $ lookupEnv "ITERATION_MULTIPLIER"
    let iterations = baseIterations * (read $ fromMaybe "1" multiplier)
    _ <- replicateM_ (fromInteger $ toInteger iterations) block  -- warm up
    afterOne <- liftIO $ getUsedMemory
    let upperBound = round $ fromIntegral afterOne * maxRatio
    go iterations upperBound
    where
    go i upperBound | i <= 0 = return $ assertString ""
                    | otherwise       = do
        _ <- block
        after <- liftIO $ getUsedMemory
        if after > upperBound
        then return $ assertString $
          "Memory leak found in iteration  " ++ show i ++ " " ++ show (after, upperBound)
        else go (i-1) upperBound
