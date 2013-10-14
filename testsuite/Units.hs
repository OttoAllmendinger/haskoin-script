module Units (tests) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Control.Monad
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
    [ testGroup "Canonical Signatures" $
        (map canonicalVectorsMap $ zip canonicalVectors [0..]) ++
        (map notCanonicalVectorsMap $ zip notCanonicalVectors [0..])
    ]

canonicalVectorsMap :: (String,Int) -> Test.Framework.Test
canonicalVectorsMap (s,i) = testCase ("Canonical Sig " ++ (show i)) func
    where func = testCanonicalSig $ canonicalVectors !! i

notCanonicalVectorsMap :: (String,Int) -> Test.Framework.Test
notCanonicalVectorsMap (s,i) = testCase ("Not canonical Sig " ++ (show i)) func
    where func = testNotCanonicalSig $ notCanonicalVectors !! i

testCanonicalSig :: String -> Assertion
testCanonicalSig str = do
    let res = decodeCanonicalSig bs
    when (isLeft res) $ liftIO $ print res
    assertBool "    > Canonical Sig" $ isRight res
    where bs = fromJust $ hexToBS str

testNotCanonicalSig :: String -> Assertion
testNotCanonicalSig str = assertBool "    > Not canonical sig" $
    isLeft $ decodeCanonicalSig bs
    where bs = fromJust $ hexToBS str

{- Canonical Signatures -}

-- Test vectors from bitcoind
-- http://github.com/bitcoin/bitcoin/blob/master/src/test/data/sig_canonical.json

canonicalVectors :: [String]
canonicalVectors =
    [ "300602010102010101" -- Changed 0x00 to 0x01 as 0x00 is invalid
    , "3008020200ff020200ff01"
    , "304402203932c892e2e550f3af8ee4ce9c215a87f9bb831dcac87b2838e2c2eaa891df0c022030b61dd36543125d56b9f9f3a1f9353189e5af33cdda8d77a5209aec03978fa001"
    , "30450220076045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"
    , "3046022100876045be6f9eca28ff1ec606b833d0b87e70b2a630f5e3a496b110967a40f90a0221008fffd599910eefe00bc803c688c2eca1d2ba7f6b180620eaa03488e6585db6ba01"
    ]

notCanonicalVectors :: [String]
notCanonicalVectors =
    [ "30050201ff020001"
    , "30470221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105022200002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed11"
    , "314402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "301f01205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb101"
    , "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed0001"
    , "304401205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "3024020002202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304402208990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "30450221005990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610502202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba610501202d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "302402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba6105020001"
    , "304402205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050220fd5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    , "304502205990e0584b2b238e1dfaad8d6ed69ecc1a4a13ac85fc0b31d0df395eb1ba61050221002d5876262c288beb511d061691bf26777344b702b00f8fe28621fe4e566695ed01"
    ]

