
module Main where

import System.Log.Logger hiding (logM)
import System.Log.Handler.Simple
import System.IO

import Control.Concurrent.Longrun

main :: IO ()
main = do

    -- setup console logging
    updateGlobalLogger rootLoggerName (System.Log.Logger.setLevel DEBUG . removeHandler)
    hConsole <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (addHandler hConsole)

    runApp $ do
        logM INFO "startup"
        _ticker <- spawnProcess "ticker" $ forever $ do
            logM INFO "tick"
            sleep 1.0
        rest

