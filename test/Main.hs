module Main where

import Test.Framework (defaultMain)
import TestVariable (testVariable)


main :: IO ()
main = defaultMain
    [ testVariable
    ]
