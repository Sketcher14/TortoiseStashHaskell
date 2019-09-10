import Test.Tasty
import Test.Tasty.HUnit
import Criterion.Main

import SBoxSpec (runTests)
import ExpandedKeySpec (runTests)
import EncryptionDecryptionSpec (runTests)
import AddRoundKeySpec (runTests)
import ShiftRowsSpec (runTests)
import MixColumnsSpec (runTests)
import PerformanceSpec (runTests)

main :: IO ()
main = Test.Tasty.defaultMain $ testGroup "Tests"
  [ SBoxSpec.runTests
  , ExpandedKeySpec.runTests
  , EncryptionDecryptionSpec.runTests
  , AddRoundKeySpec.runTests
  , ShiftRowsSpec.runTests
  , MixColumnsSpec.runTests
  ]