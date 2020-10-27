module TestSuite3 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe3 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1
    ]