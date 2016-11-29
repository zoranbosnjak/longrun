
module Main where

import System.Log.Logger hiding (logM)

import Control.Concurrent.Longrun
import Testrun

main :: IO ()
main = runScenario
    [ ("var1", var1)
    , ("var2", var2)
    ]

-- | Basic variable
var1 :: Process ()
var1 = do
    var <- newVar "var1" "content"
    val1 <- getVar' var
    logM INFO $ show val1
    setVar' var "new content"
    val2 <- getVar' var
    logM INFO $ show val2

-- | Modify variable
var2 :: Process ()
var2 = do
    var <- newVar "var" (1::Int)
    (oldVal,newVal) <- modifyVar var (+1)
    val <- getVar' var
    logM INFO $ show oldVal
    logM INFO $ show newVal
    logM INFO $ show val

