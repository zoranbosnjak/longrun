module Main where

import Control.Monad hiding (forever)
import System.Log.Logger hiding (logM)
import System.Random

import Control.Concurrent.Longrun
import Testrun

main :: IO ()
main = runScenario
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

    _src <- spawnProcess "source" nop $ forM_ samples $ \x -> do
        writeQueue' q x
        sleep 0.001

    _sink <- spawnProcess "sink" nop $ forever $ do
        _ <- readQueue' q
        return ()

    rest

