module Haskoin.Script.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Script
import Haskoin.Script.Arbitrary
import Haskoin.Crypto
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol
import Haskoin.Protocol.Arbitrary
import Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "Script Parser"
        [ testProperty "canonical signatures" testCanonicalSig
        , testProperty "decode( encode(sighash) ) = sighash" binSigHash
        , testProperty "encodeSigHash32 is 4 bytes long" testEncodeSH32
        , testProperty "decode( encode(s) ) = s" binTxSig
        , testProperty "decodeCanonical( encode(s) ) = s" binTxSigCanonical
        , testProperty "encode decode OP_1 .. OP_16" testScriptOpInt
        , testProperty "encode decode ScriptOutput" testScriptOutput
        , testProperty "encode decode ScriptInput" testScriptInput
        , testProperty "encode decode ScriptHashInput" testScriptHashInput
        ]
    ]

{- Script Parser -}

testCanonicalSig :: TxSignature -> Bool
testCanonicalSig ts = 
    isRight (decodeCanonicalSig bs) && isCanonicalHalfOrder (txSignature ts)
    where bs = encode' ts

binSigHash :: SigHash -> Bool
binSigHash sh = (decode' $ encode' sh) == sh

testEncodeSH32 :: SigHash -> Bool
testEncodeSH32 sh = BS.length bs == 4 && BS.head bs /= 0 && BS.tail bs == zs
    where bs = encodeSigHash32 sh
          zs = BS.pack [0,0,0]

binTxSig :: TxSignature -> Bool
binTxSig ts = (decode' $ encode' ts) == ts

binTxSigCanonical :: TxSignature -> Bool
binTxSigCanonical ts = (fromRight $ decodeCanonicalSig $ encode' ts) == ts

testScriptOpInt :: ScriptOpInt -> Bool
testScriptOpInt (ScriptOpInt i) = (intToScriptOp <$> scriptOpToInt i) == Right i

testScriptOutput :: ScriptOutput -> Bool
testScriptOutput so = (decodeOutput $ encodeOutput so) == Right so

testScriptInput :: ScriptInput -> Bool
testScriptInput si = (decodeInput $ encodeInput si) == Right si

testScriptHashInput :: ScriptHashInput -> Bool
testScriptHashInput sh = (decodeScriptHash $ encodeScriptHash sh) == Right sh


