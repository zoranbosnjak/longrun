module Main where

import Test.Framework (defaultMain)
import TestVariable (testVariable)
import TestWait (testWait)


main :: IO ()
main = defaultMain
    [ testVariable
    , testWait
    ]
