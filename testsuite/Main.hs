module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import qualified Haskoin.Script.Tests (tests)
import qualified Units (tests)

main = defaultMain
    (  Haskoin.Script.Tests.tests 
    ++ Units.tests 
    )

