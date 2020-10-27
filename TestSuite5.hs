module TestSuite5 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe5 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1
    ]
