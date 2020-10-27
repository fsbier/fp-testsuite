module TestSuite4 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe4 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1
    ]