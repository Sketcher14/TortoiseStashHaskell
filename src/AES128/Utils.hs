module AES128.Utils
  ( Block(Block)
  , Key(Key128)
  , AESState(..)
  , aesPolynomial
  , amountRounds
  , blockSize
  , aesMultiply
  , aesAdd
  , blockToState
  , stateToBlock
  , passwordHash
  , splitByBlocks
  , unionBlocks
  ) where

import           Control.Monad.State.Lazy
import qualified Crypto.Hash.MD5          as MD5
import           Crypto.Number.F2m        (addF2m, mulF2m)
import           Data.Bits                (xor)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import           Data.List
import           Data.List.Split          (chunksOf)
import           Data.Word
import qualified Data.ByteString.UTF8 as BSU

newtype Block =
  Block [Word8]
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

amountRounds :: Int
amountRounds = 10

blockSize :: Int
blockSize = 16

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

passwordHash :: String -> IO Key
passwordHash pass = return (Key128 (B.unpack $ MD5.hash $ BSU.fromString pass))

blockAdditionPKCS7 :: Block -> Block
blockAdditionPKCS7 bl@(Block bytes) =
  if blockLen == blockSize
    then bl
    else Block (bytes ++ replicate amountAddBytes (fromIntegral amountAddBytes))
  where
    blockLen = length bytes
    amountAddBytes = blockSize - blockLen

splitByBlocks :: L.ByteString -> [Block]
splitByBlocks input = blocks ++ [blockAdditionPKCS7 last]
  where
    (last:blocks) = reverse $ map Block $ chunksOf blockSize $ L.unpack input

removeBlockAdditionPKCS7 :: Block -> Block
removeBlockAdditionPKCS7 bl@(Block bytes) =
  if lastByte >= blockSize
    then bl
    else Block (take (blockSize - lastByte) bytes)
  where
    lastByte = fromIntegral $ last bytes

unionBlocks :: [Block] -> L.ByteString
unionBlocks inputBlocks = L.pack $ concatMap (\(Block bytes) -> bytes) (blocks ++ [removeBlockAdditionPKCS7 last])
  where
    (last:blocks) = reverse inputBlocks
