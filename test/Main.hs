module Main where

import Test.Framework (defaultMain)

-- import TestLongrun (testLongrun)
import TestQueue (testQueue)
-- import TestSubprocess (testSubprocess)
import TestTimer (testTimer)
import TestVariable (testVariable)
-- import TestWait (testWait)

main :: IO ()
main = defaultMain
    [-- testLongrun
    testQueue
    -- , testSubprocess
    , testTimer
    , testVariable
    -- , testWait
    ]
