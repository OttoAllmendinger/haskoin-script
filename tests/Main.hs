module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Network.Haskoin.Script.Tests (tests)
import qualified Units (tests)

main = defaultMain
    (  Network.Haskoin.Script.Tests.tests 
    ++ Units.tests 
    )

