module AES128.Utils
  ( Block(Block)
  , Key(Key128)
  , AESState(..)
  , aesPolynomial
  , aesMultiply
  , aesAdd
  , blockToState
  , stateToBlock
  , mcCommon
  , popSubKey
  , addRoundKey
  ) where

import           Data.Word
import           Crypto.Number.F2m        (addF2m, mulF2m)
import           Control.Monad.State.Lazy
import           Data.Bits                (xor)
import           Data.List

newtype Block = Block [Word8]
  deriving (Show) -- 16 bytes

newtype Key =
  Key128 [Word8]
  deriving (Show) -- 16 bytes

data AESState = AESState
  { w0 :: [Word8]
  , w1 :: [Word8]
  , w2 :: [Word8]
  , w3 :: [Word8]
  }

aesPolynomial :: Integer
aesPolynomial = 0x11B

aesMultiply :: Word8 -> Word8 -> Word8
aesMultiply w1 w2 = fromIntegral $ mulF2m aesPolynomial (fromIntegral w1) (fromIntegral w2)

aesAdd :: Word8 -> Word8 -> Word8
aesAdd w1 w2 = fromIntegral $ addF2m (fromIntegral w1) (fromIntegral w2)

blockToState :: Block -> AESState
blockToState (Block block) = AESState first second third fourth
  where
    first = take 4 block
    second = take 4 $ drop 4 block
    third = take 4 $ drop 8 block
    fourth = take 4 $ drop 12 block

stateToBlock :: AESState -> Block
stateToBlock (AESState w0 w1 w2 w3) = Block (w0 ++ w1 ++ w2 ++ w3)


mcCommon :: [[Word8]] -> AESState -> AESState
mcCommon mat st@(AESState w0 w1 w2 w3) =
  st {w0 = multMatCol mat w0, w1 = multMatCol mat w1, w2 = multMatCol mat w2, w3 = multMatCol mat w3}
  where
    multMatCol mat col = [doMult str col | str <- mat]
    doMult str col = foldl' aesAdd 0 (zipWith aesMultiply str col)


addRoundKey :: AESState -> Key -> AESState
addRoundKey st@(AESState w0 w1 w2 w3) (Key128 key) =
  st {w0 = zipWith xor w0 first, w1 = zipWith xor w1 second, w2 = zipWith xor w2 third, w3 = zipWith xor w3 fourth}
  where
    first = take 4 key
    second = take 4 $ drop 4 key
    third = take 4 $ drop 8 key
    fourth = take 4 $ drop 12 key


popSubKey :: State [Key] Key
popSubKey = do
  keys <- get
  put $ tail keys
  return $ head keys