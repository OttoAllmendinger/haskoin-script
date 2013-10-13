module Haskoin.Script.Parser
( ScriptOutput(..)
, ScriptInput(..)
, RedeemScript
, ScriptHashInput(..)
, scriptAddr
, encodeInput
, decodeInput
, encodeOutput
, decodeOutput
, encodeScriptHash
, decodeScriptHash
, intToScriptOp
, scriptOpToInt
) where

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Script.SigHash
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Util

data ScriptOutput = 
      PayPK         { runPayPubKey      :: !PubKey }
    | PayPKHash     { runPayPubKeyHash  :: !Address }
    | PayMulSig     { payMulSigKeys     :: ![PubKey]
                    , payMulSigRequired :: !Int
                    }
    | PayScriptHash { runPayScriptHash  :: !Address }
    deriving (Eq, Show)

scriptAddr :: ScriptOutput -> Address
scriptAddr = ScriptAddress . hash160 . hash256BS . encode' . encodeOutput
    
encodeOutput :: ScriptOutput -> Maybe Script
encodeOutput s = liftM Script $ case s of
    -- Pay to PubKey
    (PayPK k) -> Just [OP_PUSHDATA $ encode' k, OP_CHECKSIG]
    -- Pay to PubKey Hash Address
    (PayPKHash a) -> case a of
        (PubKeyAddress h) -> Just [ OP_DUP, OP_HASH160, OP_PUSHDATA $ encode' h
                                  , OP_EQUALVERIFY, OP_CHECKSIG 
                                  ] 
        (ScriptAddress _) -> Nothing
    -- Pay to MultiSig Keys
    (PayMulSig ps r) -> do
        guard $ r <= length ps 
        (opM,opN) <- liftM2 (,) (intToScriptOp r) (intToScriptOp $ length ps)
        let keys = map (OP_PUSHDATA . encode') ps
        return $ opM : keys ++ [opN, OP_CHECKMULTISIG]
    -- Pay to Script Hash Address
    (PayScriptHash a) -> case a of
        (ScriptAddress h) -> Just [ OP_HASH160
                                  , OP_PUSHDATA $ encode' h, OP_EQUAL
                                  ]
        (PubKeyAddress _) -> Nothing

decodeOutput :: Script -> Maybe ScriptOutput
decodeOutput s = case runScript s of
    -- Pay to PubKey
    [OP_PUSHDATA k, OP_CHECKSIG] -> decodeEither k Nothing (Just . PayPK)
    -- Pay to PubKey Hash
    [OP_DUP, OP_HASH160, OP_PUSHDATA h, OP_EQUALVERIFY, OP_CHECKSIG] -> 
        decodeEither h Nothing (Just . PayPKHash . PubKeyAddress)
    -- Pay to Script Hash
    [OP_HASH160, OP_PUSHDATA h, OP_EQUAL] -> 
        decodeEither h Nothing (Just . PayScriptHash . ScriptAddress)
    -- Pay to MultiSig Keys
    _ -> matchPayMulSig s

-- Match [ OP_N, PubKey1, ..., PubKeyM, OP_M, OP_CHECKMULTISIG ]
matchPayMulSig :: Script -> Maybe ScriptOutput
matchPayMulSig s@(Script ops) = case splitAt (length ops - 2) ops of
    (m:xs,[n,OP_CHECKMULTISIG]) -> do
        (intM,intN) <- liftM2 (,) (scriptOpToInt m) (scriptOpToInt n)
        guard $ intM <= intN && length xs == intN 
        liftM2 PayMulSig (go xs) (Just intM)
    _ -> Nothing
    where go (OP_PUSHDATA bs:xs) = 
              decodeEither bs Nothing $ \pub -> liftM2 (:) (Just pub) (go xs)
          go [] = Just []
          go  _ = Nothing

-- Decode OP_1 to OP_16
intToScriptOp :: Int -> Maybe ScriptOp
intToScriptOp i
    | i `elem` [1..16] = Just op
    |        otherwise = Nothing
    where op = decode' $ BS.singleton $ fromIntegral $ i + 0x50

-- Encode OP_1 to OP_16
scriptOpToInt :: ScriptOp -> Maybe Int
scriptOpToInt s 
    | res `elem` [1..16] = Just res
    | otherwise          = Nothing
    where res = (fromIntegral $ BS.head $ encode' s) - 0x50

data ScriptInput = 
      SpendPK     { runSpendPK        :: !TxSignature }
    | SpendPKHash { runSpendPKHashSig :: !TxSignature 
                  , runSpendPKHashKey :: !PubKey
                  }
    | SpendMulSig { runSpendMulSigs   :: ![TxSignature] 
                  , runRequiredSigs   :: !Int
                  }
    deriving (Eq, Show)

encodeInput :: ScriptInput -> Maybe Script
encodeInput s = liftM Script $ case s of
    -- Spend PubKey Input
    (SpendPK s) -> Just [OP_PUSHDATA $ encode' s]
    -- Spend PubKey Hash Input
    (SpendPKHash ts p) -> Just [ OP_PUSHDATA $ encode' ts
                               , OP_PUSHDATA $ encode' p
                               ]
    -- Spend MultiSig Input
    (SpendMulSig ts r) -> do
        guard $ length ts <= r && length ts <= 16
        let sigs = map (OP_PUSHDATA . encode') ts
        return $ OP_0 : sigs ++ replicate (r - length ts) OP_0

decodeInput :: Script -> Maybe ScriptInput
decodeInput s = case runScript s of
    [OP_PUSHDATA s] -> decodeEither s Nothing (Just . SpendPK)
    [OP_PUSHDATA a, OP_PUSHDATA b] -> 
        decodeEither a Nothing $ \s -> 
        decodeEither b Nothing $ \p -> Just $ SpendPKHash s p
    (OP_0 : xs) -> matchSpendMulSig $ Script xs
    _ -> Nothing

matchSpendMulSig :: Script -> Maybe ScriptInput
matchSpendMulSig (Script ops) = liftM2 SpendMulSig (go ops) (Just $ length ops)
    where go (OP_PUSHDATA bs:xs) = decodeEither bs Nothing $ 
            \sig -> liftM2 (:) (Just sig) (go xs)
          go (OP_0:xs) = if all (== OP_0) xs then Just [] else Nothing
          go [] = Just []
          go _  = Nothing

type RedeemScript = ScriptOutput

data ScriptHashInput = ScriptHashInput 
    { spendSHInput  :: ScriptInput 
    , spendSHOutput :: RedeemScript
    } deriving (Eq, Show)

encodeScriptHash :: ScriptHashInput -> Maybe Script
encodeScriptHash (ScriptHashInput i o) = do
    (Script i') <- encodeInput i
    (Script o') <- encodeOutput o
    return $ Script $ i' ++ [OP_PUSHDATA $ runPut' $ putScriptOps o']

decodeScriptHash :: Script -> Maybe ScriptHashInput
decodeScriptHash (Script ops) = case splitAt (length ops - 1) ops of
    (is,[OP_PUSHDATA bs]) -> runGetEither getScriptOps bs Nothing $ \os ->
        ScriptHashInput <$> (decodeInput  $ Script is) 
                        <*> (decodeOutput $ Script os)
    _ -> Nothing

