module Haskoin.Script.SigHash
( SigHash(..)
, encodeSigHash32
, txSigHash
, TxSignature(..)
, decodeCanonicalSig
) where

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

data SigHash = SigAll    
             | SigNone   
             | SigSingle 
             -- Anyone Can Pay
             | SigAllAcp
             | SigNoneAcp
             | SigSingleAcp 
             deriving (Eq, Show)

instance Binary SigHash where

    get = do
        w <- getWord8
        case w of 0x01 -> return SigAll
                  0x02 -> return SigNone
                  0x03 -> return SigSingle
                  0x81 -> return SigAllAcp
                  0x82 -> return SigNoneAcp
                  0x83 -> return SigSingleAcp
                  _    -> fail "Non-canonical signature: unknown hashtype byte"

    put sh = putWord8 $ case sh of
        SigAll       -> 0x01
        SigNone      -> 0x02
        SigSingle    -> 0x03
        SigAllAcp    -> 0x81
        SigNoneAcp   -> 0x82
        SigSingleAcp -> 0x83

encodeSigHash32 :: SigHash -> BS.ByteString
encodeSigHash32 sh = encode' sh `BS.append` BS.pack [0,0,0]

-- Build hash that will be used for signing a transaction
txSigHash :: Tx -> Script -> Int -> SigHash -> Either String Hash256
txSigHash tx out i sh = do
    newIn  <- buildInputs (txIn tx) out i sh
    newOut <- buildOutputs (txOut tx) i sh
    let newTx = tx{ txIn = newIn, txOut = newOut }
    return $ doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> Either String [TxIn]
buildInputs txins out i sh
    | i >= length txins = Left $ "buildInputs: index out of range " ++ (show i)
    | sh `elem` [SigAllAcp, SigNoneAcp, SigSingleAcp] =
            return $ (txins !! i) { scriptInput = out } : []
    | sh == SigAll = return single
    | sh `elem` [SigNone, SigSingle] = return $ map noSeq $ zip single [0..]
    where empty  = map (\ti -> ti{ scriptInput = Script [] }) txins
          single = updateIndex i empty $ \ti -> ti{ scriptInput = out }
          noSeq (ti,j) = if i == j then ti else ti{ txInSequence = 0 }

-- Build transaction outputs for computing SigHashes
buildOutputs :: [TxOut] -> Int -> SigHash -> Either String [TxOut]
buildOutputs txos i sh
    | sh `elem` [SigAll, SigAllAcp]       = return txos
    | sh `elem` [SigNone, SigNoneAcp]     = return []
    | sh `elem` [SigSingle, SigSingleAcp] = 
        if i < 0 || i >= length txos
            then Left $ "buildOutputs: index out of range: " ++ (show i)
            else return $ buffer ++ [txos !! i]
    where buffer = replicate i $ TxOut (-1) $ Script []

-- Signatures in scripts contain the signature hash type byte
data TxSignature = TxSignature 
    { txSignature :: Signature 
    , sigHashType :: SigHash
    } deriving (Eq, Show)

instance Binary TxSignature where
    get = liftM2 TxSignature get get
    put (TxSignature s h) = put s >> put h

-- github.com/bitcoin/bitcoin/blob/master/src/script.cpp
-- from function IsCanonicalSignature
decodeCanonicalSig :: BS.ByteString -> Either String TxSignature
decodeCanonicalSig bs
    | len < 9 = Left "Non-canonical signature: too short"
    | len > 73 = Left "Non-canonical signature: too long"
    | hashtype < 1 || hashtype > 3 = 
        Left" Non-canonical signature: unknown hashtype byte"
    | BS.index bs 0 /= 0x30 = Left "Non-canonical signature: wrong type"
    | BS.index bs 1 /= len - 3 = 
        Left "Non-canonical signature: wrong length marker"
    | 5 + rlen >= len = Left "Non-canonical signature: S length misplaced"
    | rlen + slen + 7 /= len = 
        Left "Non-canonical signature: R+S length mismatch"
    | BS.index bs 2 /= 0x02 = 
        Left "Non-canonical signature: R value type mismatch"
    | rlen == 0 = Left "Non-canonical signature: R length is zero"
    | testBit (BS.index bs 4) 7 = 
        Left "Non-canonical signature: R value negative"
    | rlen > 1 && BS.index bs 4 == 0 && not (testBit (BS.index bs 5) 7) =
        Left "Non-canonical signature: R value excessively padded"
    | BS.index bs (fromIntegral rlen+4) /= 0x02 =
        Left "Non-canonical signature: S value type mismatch"
    | slen == 0 = Left "Non-canonical signature: S length is zero"
    | testBit (BS.index bs (fromIntegral rlen+6)) 7 =
        Left "Non-canonical signature: S value negative"
    | slen > 1 && BS.index bs (fromIntegral rlen+6) == 0 
        && not (testBit (BS.index bs (fromIntegral rlen+7)) 7) =
        Left "Non-canonical signature: S value excessively padded"
    | otherwise = return $ decode' bs
    where len = fromIntegral $ BS.length bs
          rlen = BS.index bs 3
          slen = BS.index bs (fromIntegral rlen + 5)
          hashtype = clearBit (BS.last bs) 7

