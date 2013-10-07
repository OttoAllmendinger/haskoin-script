module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad.Trans

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto
import Haskoin.Util

tests =
    [ testGroup "Canonical Signatures" 
        [ testCase "Canonical Sig 1" testCanonicalSig1
        , testCase "Canonical Sig 2" testCanonicalSig2
        , testCase "Canonical Sig 3" testCanonicalSig3
        , testCase "Canonical Sig 4" testCanonicalSig4
        , testCase "Canonical Sig 5" testCanonicalSig5
        , testCase "Not Canonical Sig 1" testNotCanonical1
        , testCase "Not Canonical Sig 2" testNotCanonical2
        , testCase "Not Canonical Sig 3" testNotCanonical3
        , testCase "Not Canonical Sig 4" testNotCanonical4
        , testCase "Not Canonical Sig 5" testNotCanonical5
        , testCase "Not Canonical Sig 6" testNotCanonical6
        , testCase "Not Canonical Sig 7" testNotCanonical7
        , testCase "Not Canonical Sig 8" testNotCanonical8
        , testCase "Not Canonical Sig 9" testNotCanonical9
        , testCase "Not Canonical Sig 10" testNotCanonical10
        , testCase "Not Canonical Sig 11" testNotCanonical11
        , testCase "Not Canonical Sig 12" testNotCanonical12
        , testCase "Not Canonical Sig 13" testNotCanonical13
        , testCase "Not Canonical Sig 14" testNotCanonical14
        , testCase "Not Canonical Sig 15" testNotCanonical15
        ] 
    ]

{- Canonical Signatures -}

-- Test vectors from bitcoind
-- http://github.com/bitcoin/bitcoin/blob/master/src/test/data/sig_canonical.json

testCanonicalSig1 = assertBool "Canonical Sig1" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "300602010002010001"

testCanonicalSig2 = assertBool "Canonical Sig2" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "3008020200ff020200ff01"

testCanonicalSig3 = assertBool "Canonical Sig3" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "304402203932c892e2e550f3af8ee4ce9c215a87f9bb831dcac87b2838e2c2eaa891df0c022030b61dd36543125d56b9f9f3a1f9353189e5af33cdda8d77a5209aec03978fa001"

testCanonicalSig4 = assertBool "Canonical Sig4" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "30450220076045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"

testCanonicalSig5 = assertBool "Canonical Sig5" $ 
    isCanonicalSig $ fromJust $ hexToBS $ stringToBS 
    "3046022100876045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"

testNotCanonical1 = assertBool "Too short" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "30050201ff020001"

testNotCanonical2 = assertBool "Too long" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "30470221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105022200002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical3 = assertBool "Hashtype" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed11"

testNotCanonical4 = assertBool "Type" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "314402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical5 = assertBool "Total length" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical6 = assertBool "S length OOB" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "301f01205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb101"

testNotCanonical7 = assertBool "R+S" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed0001"

testNotCanonical8 = assertBool "R type" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304401205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical9 = assertBool "R len = 0" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "3024020002202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical10 = assertBool "R < -" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402208990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical11 = assertBool "R padded" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "30450221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical12 = assertBool "S type" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610501202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical13 = assertBool "S len = 0" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "302402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105020001"

testNotCanonical14 = assertBool "S < 0" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050220fd5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"

testNotCanonical15 = assertBool "S padded" $
    not $ isCanonicalSig $ fromJust $ hexToBS $ stringToBS
    "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050221002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"


