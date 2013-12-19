module Main where

import Test.Framework (defaultMain)

import qualified Network.Haskoin.Script.Tests (tests)
import qualified Units (tests)

main :: IO ()
main = defaultMain
    (  Network.Haskoin.Script.Tests.tests 
    ++ Units.tests 
    )

