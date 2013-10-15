module Haskoin.Script.SigHash
( SigHash(..)
, encodeSigHash32
, isSigAll
, isSigNone
, isSigSingle
, isSigUnknown
, txSigHash
, TxSignature(..)
, encodeSig
, decodeSig
, decodeCanonicalSig
) where

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

data SigHash = SigAll     { anyoneCanPay :: Bool }   
             | SigNone    { anyoneCanPay :: Bool }     
             | SigSingle  { anyoneCanPay :: Bool }   
             | SigUnknown { anyoneCanPay :: Bool
                          , runSigUnknown :: Word8 
                          }
             deriving (Eq, Show)

isSigAll :: SigHash -> Bool
isSigAll sh = case sh of
    SigAll _ -> True
    _ -> False

isSigNone :: SigHash -> Bool
isSigNone sh = case sh of
    SigNone _ -> True
    _ -> False

isSigSingle :: SigHash -> Bool
isSigSingle sh = case sh of
    SigSingle _ -> True
    _ -> False

isSigUnknown :: SigHash -> Bool
isSigUnknown sh = case sh of
    SigUnknown _ _ -> True
    _ -> False

instance Binary SigHash where

    get = getWord8 >>= \w ->
        let acp = testBit w 7
            in return $ case clearBit w 7 of
                1 -> SigAll acp
                2 -> SigNone acp
                3 -> SigSingle acp
                _ -> SigUnknown acp w

    put sh = putWord8 $ case sh of
        SigAll acp -> if acp then 0x81 else 0x01
        SigNone acp -> if acp then 0x82 else 0x02
        SigSingle acp -> if acp then 0x83 else 0x03
        SigUnknown _ w -> w

encodeSigHash32 :: SigHash -> BS.ByteString
encodeSigHash32 sh = encode' sh `BS.append` BS.pack [0,0,0]

-- Build hash that will be used for signing a transaction
txSigHash :: Tx -> Script -> Int -> SigHash -> Hash256
txSigHash tx out i sh = do
    let newIn = buildInputs (txIn tx) out i sh
    -- When SigSingle and input index > outputs, then sign integer 1
    fromMaybe (setBit 0 248) $ do
        newOut <- buildOutputs (txOut tx) i sh
        let newTx = tx{ txIn = newIn, txOut = newOut }
        return $ doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh

-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh
    | anyoneCanPay sh   = (txins !! i) { scriptInput = out } : []
    | isSigAll sh || isSigUnknown sh = single
    | otherwise         = map noSeq $ zip single [0..]
    where empty  = map (\ti -> ti{ scriptInput = Script [] }) txins
          single = updateIndex i empty $ \ti -> ti{ scriptInput = out }
          noSeq (ti,j) = if i == j then ti else ti{ txInSequence = 0 }

-- Build transaction outputs for computing SigHashes
buildOutputs :: [TxOut] -> Int -> SigHash -> Maybe [TxOut]
buildOutputs txos i sh
    | isSigAll sh || isSigUnknown sh = return txos
    | isSigNone sh = return []
    | i >= length txos = Nothing
    | otherwise = return $ buffer ++ [txos !! i]
    where buffer = replicate i $ TxOut (-1) $ Script []

-- Signatures in scripts contain the signature hash type byte
data TxSignature = TxSignature 
    { txSignature :: Signature 
    , sigHashType :: SigHash
    } deriving (Eq, Show)

encodeSig :: TxSignature -> BS.ByteString
encodeSig (TxSignature sig sh) = runPut' $ put sig >> put sh

decodeSig :: BS.ByteString -> Either String TxSignature
decodeSig bs = do
    let (h,l) = BS.splitAt (BS.length bs - 1) bs
    liftM2 TxSignature (decodeToEither h) (decodeToEither l)

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
    | otherwise = decodeSig bs
    where len = fromIntegral $ BS.length bs
          rlen = BS.index bs 3
          slen = BS.index bs (fromIntegral rlen + 5)
          hashtype = clearBit (BS.last bs) 7

