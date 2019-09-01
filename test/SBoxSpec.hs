module SBoxSpec
  ( runTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import AES128.SBox

runTests :: TestTree
runTests = testGroup " SBox module" [subByteTest, invSubByteTest, isReverseTables]

subByteTest :: TestTree
subByteTest =
  testCase
    "Tests some specifc sBox outputs for correctness" $ do
      assertEqual "Test01" (subByte 0x21) 0xfd
      assertEqual "Test02" (subByte 0xba) 0xf4
      assertEqual "Test03" (subByte 0x5b) 0x39
      assertEqual "Test04" (subByte 0x77) 0xf5

invSubByteTest :: TestTree
invSubByteTest =
  testCase
    "Tests some specifc invSBox outputs for correctness" $ do
      assertEqual "Test01" (invSubByte 0xba) 0xc0
      assertEqual "Test02" (invSubByte 0x5c) 0xa7
      assertEqual "Test03" (invSubByte 0xe2) 0x3b
      assertEqual "Test04" (invSubByte 0x0f) 0xfb

isReverseTables :: TestTree
isReverseTables =
  testCase
    "Ensures sBox and invSBox are truly inverses" $
     assertEqual "Test01" (all (\x -> x == invSubByte (subByte x)) [0x00..0xff]) True