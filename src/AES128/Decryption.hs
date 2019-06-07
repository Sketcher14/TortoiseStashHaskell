module AES128.Decryption where

import           AES128.SBox
import           AES128.Utils
import           AES128.ExpandedKey
import           Data.Word
import           Control.Monad.State.Lazy
import           Data.Bits                (xor)
import           Data.List

invSubWord :: [Word8] -> [Word8]
invSubWord = map invSubByte

invSubBytes :: AESState -> AESState
invSubBytes st@(AESState w0 w1 w2 w3) =
  st {w0 = invSubWord w0, w1 = invSubWord w1, w2 = invSubWord w2, w3 = invSubWord w3}

invShiftRows :: AESState -> AESState --toRight
invShiftRows st@(AESState w0 w1 w2 w3) = shift w0 w1 w2 w3
  where
    shift [w00, w01, w02, w03] [w10, w11, w12, w13] [w20, w21, w22, w23] [w30, w31, w32, w33] =
      st {w0 = [w00, w31, w22, w13], w1 = [w10, w01, w32, w23], w2 = [w20, w11, w02, w33], w3 = [w30, w21, w12, w03]}



invMixColumns :: AESState -> AESState
invMixColumns = mcCommon matrixConst
  where
    matrixConst =
      [[0x0E, 0x0B, 0x0D, 0x09], [0x09, 0x0E, 0x0B, 0x0D], [0x0D, 0x09, 0x0E, 0x0B], [0x0B, 0x0D, 0x09, 0x0E]]


roundDecrypt :: AESState -> Key -> AESState
roundDecrypt state key = invSubBytes $ invShiftRows $ invMixColumns $ addRoundKey state key

finalRoundDecrypt :: AESState -> Key -> AESState
finalRoundDecrypt state key = invSubBytes $ invShiftRows $ addRoundKey state key


decryptStateful :: Int -> AESState -> State [Key] AESState
decryptStateful 0 state = return state
decryptStateful n state
  | n == 10 = do
    subKey <- popSubKey
    decryptStateful (n - 1) $ finalRoundDecrypt state subKey
  | n < 10 = do
    subKey <- popSubKey
    decryptStateful (n - 1) $ roundDecrypt state subKey

decrypt :: Block -> Key -> Block
decrypt block key = stateToBlock $ addRoundKey (evalState stateMonad reversedKeys) key
  where
    aesState = blockToState block
    reversedKeys = reverse $ generateExpandedKey key
    stateMonad = decryptStateful 10 aesState
