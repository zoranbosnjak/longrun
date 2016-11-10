module Main where

import Control.Monad hiding (forever)
import System.Environment (getArgs)
import System.Log.Logger hiding (logM)
import System.Log.Handler.Simple
import System.IO
import System.Random

import Control.Concurrent.Longrun

main :: IO ()
main = do
    args <- getArgs

    -- setup console logging
    updateGlobalLogger rootLoggerName 
        (System.Log.Logger.setLevel DEBUG . removeHandler)
    hConsole <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (addHandler hConsole)

    -- run all tests from command line
    forM_ args $ \arg -> do
        putStrLn ""
        putStrLn $ "running: " ++ show arg
        let mFunc = lookup arg scenarios
        case mFunc of
            Nothing -> error $ show arg ++ " not found."
            Just func -> runApp func

scenarios :: [(String, Process ())]
scenarios =
    [ ("chvar", chvar)
    , ("long", long)
    ]

-- | Variable change
chvar :: Process ()
chvar = do
    var <- newVar "var" (1::Int)
    _ <- onChangeVar "observer" 1 (GetEnd var) id $ \oldVal newVal -> do
        logM INFO $ "Variable changed " ++ show oldVal ++ " -> " ++ show newVal

    sleep 0.1
    setVar' var 2
    sleep 0.1
    setVar' var 3
    sleep 0.1

long :: Process ()
long = do
    g <- runIO newStdGen
    let samples :: [Int]
        samples = randoms g

    q <- newQueue1 "q"

    _src <- spawnProcess "source" $ forM_ samples $ \x -> do
        writeQueue' q x
        sleep 0.001

    _sink <- spawnProcess "sink" $ forever $ do
        _ <- readQueue' q
        return ()

    rest

