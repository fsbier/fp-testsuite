module TestSuite6 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe6 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1
    ]