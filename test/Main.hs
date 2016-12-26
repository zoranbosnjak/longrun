module Main where

import Test.Framework (defaultMain)
import TestQueue (testQueue)
import TestSubprocess (testSubprocess)
import TestTimer (testTimer)
import TestVariable (testVariable)
import TestWait (testWait)


main :: IO ()
main = defaultMain
    [ testQueue
    , testSubprocess
    , testTimer
    , testVariable
    , testWait
    ]
