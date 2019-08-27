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

block1 :: Block
block1 = Block [0x6B, 0xC1, 0xBE, 0xE2, 0x2E, 0x40, 0x9F, 0x96, 0xE9, 0x3D, 0x7E, 0x11, 0x73, 0x93, 0x17, 0x2A]

key :: Key
key = Key128 [0x2B, 0x7E, 0x15, 0x16, 0x28, 0xAE, 0xD2, 0xA6, 0xAB, 0xF7, 0x15, 0x88, 0x09, 0xCF, 0x4F, 0x3C]

prettyPrint :: [Word8] -> String
prettyPrint words8 = concatMap (`showHex` " ") . B.unpack $ B.pack words8

main :: IO ()
main = mainWindow
  {-input <- B.readFile "input.txt"
  passHash <- passwordHash "hello"
  B.writeFile "out.txt" $ unionBlocks (encrypt (splitByBlocks input) passHash)-}
  {-readDecryptWrite "input.ts" "input2.txt" "glamozda"-}
  {-input <- B.readFile "input.ts"
  passHash <- passwordHash "glamozda"
  B.writeFile "input2.txt" $ unionBlocks (readDecryptWrite (splitByBlocks input) passHash)-}
