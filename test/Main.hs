module Main where

import Test.Framework (defaultMain)
import TestQueue (testQueue)
import TestVariable (testVariable)
import TestWait (testWait)


main :: IO ()
main = defaultMain
    [ testVariable
    , testWait
    , testQueue
    ]
