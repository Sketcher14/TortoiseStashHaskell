module AES128.Encryption where

import           AES128.SBox
import           AES128.Utils
import           AES128.ExpandedKey
import           Data.Word
import           Control.Monad.State.Lazy
import           Data.Bits                (xor)
import           Data.List

subWord :: [Word8] -> [Word8]
subWord = map subByte

subBytes :: AESState -> AESState
subBytes st@(AESState w0 w1 w2 w3) = st {w0 = subWord w0, w1 = subWord w1, w2 = subWord w2, w3 = subWord w3}

shiftRows :: AESState -> AESState --toLeft
shiftRows st@(AESState w0 w1 w2 w3) = shift w0 w1 w2 w3
  where
    shift [w00, w01, w02, w03] [w10, w11, w12, w13] [w20, w21, w22, w23] [w30, w31, w32, w33] =
      st {w0 = [w00, w11, w22, w33], w1 = [w10, w21, w32, w03], w2 = [w20, w31, w02, w13], w3 = [w30, w01, w12, w23]}


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
encryptStateful 1 state = do
  subKey <- popSubKey
  return $ finalRoundEncrypt state subKey
encryptStateful n state = do
  subKey <- popSubKey
  encryptStateful (n - 1) $ roundEncrypt state subKey

encrypt :: Block -> Key -> Block
encrypt block key = stateToBlock $ evalState stateMonad $ generateExpandedKey key
  where
    aesState = blockToState block
    stateMonad = encryptStateful 10 $ addRoundKey aesState key
