module Network.Haskoin.Script.Merkle where

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.ByteString as BS 

import Network.Haskoin.Protocol
import Network.Haskoin.Crypto
import Network.Haskoin.Util

maxBlockSize :: Int
maxBlockSize = 1000000

calcTreeHeight :: Int -> Int
calcTreeHeight ntx = ceiling $ log (fromIntegral ntx) / log 2

calcTreeWidth :: Int -> Int -> Int
calcTreeWidth ntx h = (ntx + (1 `shiftL` h) - 1) `shiftR` h

buildMerkleRoot :: [Hash256] -> Hash256
buildMerkleRoot txs = calcHash (calcTreeHeight $ length txs) 0 txs

hash2 :: Hash256 -> Hash256 -> Hash256
hash2 a b = doubleHash256 $ encode' a `BS.append` encode' b

calcHash :: Int -> Int -> [Hash256] -> Hash256
calcHash height pos txs
    | height < 0 || pos < 0 = error "calcHash: Invalid parameters"
    | height == 0 = txs !! pos
    | otherwise = hash2 left right
  where
    left = calcHash (height-1) (pos*2) txs
    right | pos*2+1 < calcTreeWidth (length txs) (height-1) = 
                calcHash (height-1) (pos*2+1) txs
          | otherwise = left

buildPartialMerkle :: [(Hash256,Bool)] -> ([Bool], [Hash256])
buildPartialMerkle hs = traverseAndBuild (calcTreeHeight $ length hs) 0 hs

traverseAndBuild :: Int -> Int -> [(Hash256,Bool)] -> ([Bool], [Hash256])
traverseAndBuild height pos txs
    | height < 0 || pos < 0 = error "traverseAndBuild: Invalid parameters"
    | height == 0 || not match = ([match],[calcHash height pos t])
    | otherwise = (match : lb ++ rb, lh ++ rh)
  where
    t = map fst txs
    s = pos `shiftL` height
    e = min (length txs) $ (pos+1) `shiftL` height
    match = or $ map snd $ take (e-s) $ drop s txs
    (lb,lh) = traverseAndBuild (height-1) (pos*2) txs
    (rb,rh) | (pos*2+1) < calcTreeWidth (length txs) (height-1)
                = traverseAndBuild (height-1) (pos*2+1) txs
            | otherwise = ([],[])

traverseAndExtract :: Int -> Int -> Int -> [Bool] -> [Hash256] 
                   -> Maybe (Hash256, [Hash256], Int, Int)
traverseAndExtract height pos ntx flags hashes
    | length flags == 0        = Nothing
    | height == 0 || not match = leafResult
    | isNothing leftM          = Nothing
    | (pos*2+1) >= calcTreeWidth ntx (height-1) = 
        Just (hash2 lh lh, lm, lcf+1, lch)
    | isNothing rightM         = Nothing
    | otherwise = 
        Just (hash2 lh rh, lm ++ rm, lcf+rcf+1, lch+rch)
  where
    leafResult
        | null hashes = Nothing
        | otherwise = Just (h,if height == 0 && match then [h] else [],1,1)
    (match:fs) = flags
    (h:hs)     = hashes
    leftM  = traverseAndExtract (height-1) (pos*2) ntx fs hashes
    (lh,lm,lcf,lch) = fromJust leftM
    rightM = traverseAndExtract (height-1) (pos*2+1) ntx 
                (drop lcf fs) (drop lch hashes)
    (rh,rm,rcf,rch) = fromJust rightM

extractMatches :: [Bool] -> [Hash256] -> Int
               -> Either String (Hash256, [Hash256])
extractMatches flags hashes ntx
    | ntx == 0 = Left $
        "extractMatches: number of transactions can not be 0"
    | ntx > maxBlockSize `div` 60 = Left $
        "extractMatches: number of transactions excessively high"
    | length hashes > ntx = Left $
        "extractMatches: More hashes provided than the number of transactions"
    | length flags < length hashes = Left $
        "extractMatches: At least one bit per node and one bit per hash"
    | isNothing resM = Left $
        "extractMatches: traverseAndExtract failed"
    | (nBitsUsed+7) `div` 8 /= (length flags+7) `div` 8 = Left $
        "extractMatches: All bits were not consumed"
    | nHashUsed /= length hashes = Left $
        "extractMatches: All hashes were not consumed: " ++ (show nHashUsed)
    | otherwise = return (merkleRoot, matches)
  where
    resM = traverseAndExtract (calcTreeHeight ntx) 0 ntx flags hashes
    (merkleRoot, matches, nBitsUsed, nHashUsed) = fromJust resM



