module ExpandedKeySpec
  ( runTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import AES128.ExpandedKey
import AES128.Utils

runTests :: TestTree
runTests = testGroup " ExpandedKey module" [generateExpandedKeyTest]

key :: Key
key = Key128 [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c]

subKey1 :: Key
subKey1 = Key128 [0xa0, 0xfa, 0xfe, 0x17, 0x88, 0x54, 0x2c, 0xb1, 0x23, 0xa3, 0x39, 0x39, 0x2a, 0x6c, 0x76, 0x05]

subKey3 :: Key
subKey3 = Key128 [0x3d, 0x80, 0x47, 0x7d, 0x47, 0x16, 0xfe, 0x3e, 0x1e, 0x23, 0x7e, 0x44, 0x6d, 0x7a, 0x88, 0x3b]

subKey5 :: Key
subKey5 = Key128 [0xd4, 0xd1, 0xc6, 0xf8, 0x7c, 0x83, 0x9d, 0x87, 0xca, 0xf2, 0xb8, 0xbc, 0x11, 0xf9, 0x15, 0xbc]

subKey7 :: Key
subKey7 = Key128 [0x4e, 0x54, 0xf7, 0x0e, 0x5f, 0x5f, 0xc9, 0xf3, 0x84, 0xa6, 0x4f, 0xb2, 0x4e, 0xa6, 0xdc, 0x4f]

subKey9 :: Key
subKey9 = Key128 [0xac, 0x77, 0x66, 0xf3, 0x19, 0xfa, 0xdc, 0x21, 0x28, 0xd1, 0x29, 0x41, 0x57, 0x5c, 0x00, 0x6e]

generateExpandedKeyTest :: TestTree
generateExpandedKeyTest =
  testCase "Checks given a key, it prodeces the correct list of subkeys" $ do
    assertEqual "Test01" ((!!0) . generateExpandedKey $ key) subKey1
    assertEqual "Test02" ((!!2) . generateExpandedKey $ key) subKey3
    assertEqual "Test03" ((!!4) . generateExpandedKey $ key) subKey5
    assertEqual "Test04" ((!!6) . generateExpandedKey $ key) subKey7
    assertEqual "Test05" ((!!8) . generateExpandedKey $ key) subKey9