
module Main where

import Control.Monad hiding (forever)
import System.Environment (getArgs)
import System.Log.Logger hiding (logM)
import System.Log.Handler.Simple
import System.IO

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
    [ ("queue1", queue1)
    , ("queue2", queue2)
    ]

-- | Unbounded queue
queue1 :: Process ()
queue1 = do
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

-- | Bounded queue
queue2 :: Process ()
queue2 = do
    q <- newQueue1 "q"
    writeQueue' q "a"
    readQueue' q >>= logM INFO . show 
    writeQueue' q "c"
    t <- spawnTask "writer" $ do
        writeQueue' q "b"
    rv <- waitCatch t
    logM INFO $ show rv
    logM INFO "done"

