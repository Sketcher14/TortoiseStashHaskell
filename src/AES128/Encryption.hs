module AES128.Encryption
  ( shiftRows
  , mixColumns
  , encrypt
  , readEncryptWrite
  ) where

import           AES128.ExpandedKey
import           AES128.SBox
import           AES128.Utils
import           Control.Monad.State.Lazy
import           Data.Bits                (xor)
import           Data.List
import           Data.Word
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L

subWord :: [Word8] -> [Word8]
subWord = map subByte

subBytes :: AESState -> AESState
subBytes st@(AESState w0 w1 w2 w3) = st {w0 = subWord w0, w1 = subWord w1, w2 = subWord w2, w3 = subWord w3}

shiftRows :: AESState -> AESState --toLeft
shiftRows st@(AESState w0 w1 w2 w3) = shift w0 w1 w2 w3
  where
    shift [w00, w01, w02, w03] [w10, w11, w12, w13] [w20, w21, w22, w23] [w30, w31, w32, w33] =
      st {w0 = [w00, w11, w22, w33], w1 = [w10, w21, w32, w03], w2 = [w20, w31, w02, w13], w3 = [w30, w01, w12, w23]}

mcCommon :: [[Word8]] -> AESState -> AESState
mcCommon mat st@(AESState w0 w1 w2 w3) =
  st {w0 = multMatCol mat w0, w1 = multMatCol mat w1, w2 = multMatCol mat w2, w3 = multMatCol mat w3}
  where
    multMatCol mat col = [doMult str col | str <- mat]
    doMult str col = foldl' aesAdd 0 (zipWith aesMultiply str col)

mixColumns :: AESState -> AESState
mixColumns = mcCommon matrixConst
  where
    matrixConst =
      [[0x02, 0x03, 0x01, 0x01], [0x01, 0x02, 0x03, 0x01], [0x01, 0x01, 0x02, 0x03], [0x03, 0x01, 0x01, 0x02]]

roundEncrypt :: AESState -> Key -> AESState
roundEncrypt state = addRoundKey (mixColumns $ shiftRows $ subBytes state)

finalRoundEncrypt :: AESState -> Key -> AESState
finalRoundEncrypt state = addRoundKey (shiftRows $ subBytes state)

encryptStateful :: Int -> AESState -> State [Key] AESState
encryptStateful 1 state = finalRoundEncrypt state <$> popSubKey
encryptStateful n state = do
  subKey <- popSubKey
  encryptStateful (n - 1) $ roundEncrypt state subKey

encryptBlock :: Block -> Key -> Block
encryptBlock block key = stateToBlock $ evalState stateMonad $ generateExpandedKey key
  where
    aesState = blockToState block
    stateMonad = encryptStateful amountRounds $ addRoundKey aesState key

encrypt :: [Block] -> Key -> [Block]
encrypt blocks key = map (`encryptBlock` key) blocks

readEncryptWrite :: String -> String -> String -> IO ()
readEncryptWrite decPath encPath password = do
  input <- L.readFile decPath
  passwordHash <- passwordHash password
  L.writeFile encPath $ unionBlocks $ encrypt (splitByBlocks input) passwordHash
  return ()