module TestSuite1 where

import Test.Tasty
import Test.Tasty.HUnit
import Angabe1

spec :: TestTree
spec =
  testGroup
    "Angabe1 Tests"
    [
      testCase "filtere a" $
        filtere 4 [] @?= [],
      testCase "filtere b" $
        filtere 1 [42] @?= [42],
      testCase "filtere c" $
        filtere 2 [42] @?= [],
      testCase "filtere d" $
        filtere 1 [4,-2,5,4,3,-2,5,4,-12] @?= [3,-12],
      testCase "filtere e" $
        filtere 2 [4,-2,5,4,3,-2,5,4,-12] @?= [5,-2],
      testCase "filtere f" $
        filtere 3 [4,-2,5,4,3,-2,5,4,-12] @?= [4],

      testCase "kommt vor a" $
        kommt_vor 3 [] @?= False,
      testCase "kommt vor b" $
        kommt_vor 3 [(2,3),(3,4),(5,6)] @?= True,
      testCase "kommt vor c" $
        kommt_vor 5 [(2,3),(3,4),(5,6)] @?= True,
      testCase "kommt vor d" $
        kommt_vor 6 [(2,3),(3,4),(5,6)] @?= True,
      testCase "kommt vor e" $
        kommt_vor 9 [(2,3),(3,4),(5,6)] @?= False,

      testCase "aus a" $
        aus [] @?= [],
      testCase "aus b" $
        aus [42] @?= [42],
      testCase "aus c" $
        aus [4,-2,5,4,3,-2,5,4,-12] @?= [-12,-12,-12,-2,-2,-2,3,3,3,4,4,4,5,5,5],

      testCase "hemming a" $
        h "Fahrrad" "Autobus" @?= 7,
      testCase "hemming b" $
        h "Funken" "funken" @?= 1,
      testCase "hemming c" $
        h "1001" "1111" @?= 2,
      testCase "hemming d" $
        h "Funktional" "Objektorientiert" @?= -1,
      testCase "hemming e" $
        h "12345" "13344" @?= 2,
      testCase "hemming f" $
        h "Haus" "Baum" @?= 2,


      testCase "complete" $
        1 @?= 1
    ]
