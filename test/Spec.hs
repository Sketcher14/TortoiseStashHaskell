import Test.Tasty
import Test.Tasty.HUnit

import SBoxSpec (runTests)
import ExpandedKeySpec (runTests)
import EncryptionDecryptionSpec (runTests)
import AddRoundKeySpec (runTests)
import ShiftRowsSpec (runTests)
import MixColumnsSpec (runTests)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ SBoxSpec.runTests
  , ExpandedKeySpec.runTests
  , EncryptionDecryptionSpec.runTests
  , AddRoundKeySpec.runTests
  , ShiftRowsSpec.runTests
  , MixColumnsSpec.runTests
  ]