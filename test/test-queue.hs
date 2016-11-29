
module Main where

import System.Log.Logger hiding (logM)

import Control.Concurrent.Longrun
import Testrun

main :: IO ()
main = runScenario
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

