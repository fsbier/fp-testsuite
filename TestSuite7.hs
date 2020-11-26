module TestSuite7 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe7 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1
    ]
