module Haskoin.Script.Arbitrary where

import Test.QuickCheck
import Haskoin.Crypto.Arbitrary
import Haskoin.Protocol.Arbitrary

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString as BS

import Haskoin.Script
import Haskoin.Protocol
import Haskoin.Crypto

instance Arbitrary TxSignature where
    arbitrary = liftM2 TxSignature arbitrary arbitrary

instance Arbitrary SigHash where
    arbitrary = do
        w <- arbitrary :: Gen Word8
        let acp = testBit w 7
        return $ case clearBit w 7 of
            1 -> SigAll acp
            2 -> SigNone acp
            3 -> SigSingle acp
            _ -> SigUnknown acp w

instance Arbitrary ScriptOutput where
    arbitrary = oneof 
        [ PayPK <$> arbitrary
        , (PayPKHash . pubKeyAddr) <$> arbitrary 
        , genPayMulSig
        , (PayScriptHash . scriptAddr) <$> arbitrary
        ]

genPayMulSig :: Gen ScriptOutput
genPayMulSig = do
    n <- choose (1,16)
    m <- choose (1,n)
    PayMulSig <$> (vectorOf n arbitrary) <*> (return m)

instance Arbitrary ScriptInput where
    arbitrary = oneof
        [ SpendPK <$> arbitrary
        , SpendPKHash <$> arbitrary <*> arbitrary
        , genSpendMulSig
        ]

genSpendMulSig :: Gen ScriptInput
genSpendMulSig = do
    m <- choose (1,16)
    s <- choose (1,m)
    SpendMulSig <$> (vectorOf s arbitrary) <*> (return m)

instance Arbitrary ScriptHashInput where
    arbitrary = ScriptHashInput <$> arbitrary <*> arbitrary

data ScriptOpInt = ScriptOpInt ScriptOp
    deriving (Eq, Show)

instance Arbitrary ScriptOpInt where
    arbitrary = ScriptOpInt <$> elements 
                    [ OP_1,  OP_2,  OP_3,  OP_4
                    , OP_5,  OP_6,  OP_7,  OP_8
                    , OP_9,  OP_10, OP_11, OP_12
                    , OP_13, OP_14, OP_15, OP_16
                    ]

