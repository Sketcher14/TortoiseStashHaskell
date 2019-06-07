module AES128.ExpandedKey
    ( generateExpandedKey
    ) where

import Control.Monad.State.Lazy
import Data.Word
import Data.Bits (xor)
import Data.List.Split (chunksOf)
import Crypto.Number.F2m (modF2m)
import AES128.SBox
import AES128.Utils

type KeyWord = [Word8]
type RC = Word8

generateExpandedKey :: Key -> [Key]
generateExpandedKey k = reverse keys
    where
        keys = snd . execState (keyGen 10 k) $ (1, [])

pushSubKey :: Key -> State (RC, [Key]) ()
pushSubKey k = do
    (rcs, sks) <- get
    put (rcs, k:sks)

popRC :: State (RC, [Key]) RC
popRC = do
    (rc, sks) <- get
    let newRC = fromIntegral . modF2m aesPolynomial . (2*) . fromIntegral $ rc
    put (newRC, sks)
    return rc

keyGen :: Int -> Key -> State (RC, [Key]) ()
keyGen 0 _ = return ()
keyGen n (Key128 k) = do
    rc <- popRC
    let w0' = w0 `mapXor` g rc w3
    let w1' = w0' `mapXor` w1
    let w2' = w1' `mapXor` w2
    let w3' = w2' `mapXor` w3
    let subKey = Key128 (concat [w0', w1', w2', w3'])
    pushSubKey subKey
    keyGen (n-1) subKey
        where
            mapXor = zipWith xor
            [w0, w1, w2, w3] = chunksOf 4 k

g :: RC -> KeyWord -> KeyWord
g rc kw = rcXor . map subByte $ kwRot
    where
        kwRot = let (l, r) = splitAt 1 kw in r ++ l
        rcXor (x:xs) = (rc `xor` x):xs