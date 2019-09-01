module Main where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Word
import           Numeric               (showHex)

import           AES128.Decryption
import           AES128.Encryption
import           AES128.ExpandedKey
import           AES128.Utils
import           GUI.MainWindow

prettyPrint :: [Word8] -> String
prettyPrint words8 = concatMap (`showHex` " ") . B.unpack $ B.pack words8

showKeyInHex :: Key -> String
showKeyInHex (Key128 list) = prettyPrint list

showAESStateInHex :: AESState -> String
showAESStateInHex AESState { w0 = w0, w1 = w1, w2 = w2, w3 = w3 } = prettyPrint w0 ++ prettyPrint w1 ++ prettyPrint w2 ++ prettyPrint w3

block1 :: Block
block1 = Block [0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93, 0x17, 0x2a]

key :: Key
key = Key128 [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c]

main :: IO ()
main = mainWindow
  {-input <- B.readFile "input.txt"
  passHash <- passwordHash "hello"
  B.writeFile "out.txt" $ unionBlocks (encrypt (splitByBlocks input) passHash)-}
  {-readDecryptWrite "input.ts" "input2.txt" "glamozda"-}
  {-input <- B.readFile "input.ts"
  passHash <- passwordHash "glamozda"
  B.writeFile "input2.txt" $ unionBlocks (readDecryptWrite (splitByBlocks input) passHash)-}
