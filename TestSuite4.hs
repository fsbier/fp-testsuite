module TestSuite4 where

import Test.Tasty
import Test.Tasty.HUnit

import Angabe4

spec :: TestTree
spec =
  testGroup
    "Angabe4 Tests"
    [
      --Aufgabe 1
      testCase "breitest Test 1" $ breitest (Blatt "1") @?= (Ausw 1 [0]),

      testCase "breitest Test 2" $ breitest (
        Knoten "1" 
          (Blatt "2") 
          (Blatt "3")
        ) @?= (Ausw 2 [1]),

      testCase "breitest Test 3" $ breitest (
        Knoten "1"
          (Knoten "3" 
            (Blatt "4") 
            (Blatt "5")
          ) 
          (Blatt "6")
        ) @?= (Ausw 2 [1,2]),

      testCase "breitest Test 4" $ breitest (
        Knoten "1"
          (Knoten "3" 
            (Blatt "4")
            (Blatt "5")
          )
          (Knoten "6" 
            (Blatt "7")
            (Blatt "8")
          )
        ) @?= (Ausw 4 [2]),

      testCase "breitest Test 5" $ breitest (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Blatt "9")
            (Blatt "10")
          )
        ) @?= (Ausw 4 [2]),

        testCase "breitest Test 6" $ breitest (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Blatt "9")
            (Knoten "10" 
              (Blatt "11")
              (Blatt "12")
            )
          )
        ) @?= (Ausw 4 [2, 3]),

        testCase "breitest Test 7" $ breitest (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Knoten "9" 
              (Blatt "10")
              (Blatt "11")
            )
            (Knoten "12" 
              (Blatt "13")
              (Blatt "14")
            )
          )
        ) @?= (Ausw 6 [3]),

        testCase "breitest Test 8" $ breitest (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Knoten "5" 
                (Knoten "6" 
                  (Knoten "7" 
                    (Knoten "8" 
                      (Blatt "9")
                      (Blatt "10")
                    )
                    (Blatt "11")
                  )
                  (Blatt "12")
                )
                (Blatt "13")
              )
              (Blatt "14")
            )
            (Blatt "15")
          )
          (Blatt "16")
        ) @?= (Ausw 2 [1,2,3,4,5,6,7]),



        --Aufgabe 2
        testCase "tae Test 1" $ tae (Blatt "1") 0 @?= Just ["1"],
        testCase "tae Test 2" $ tae (Blatt "1") 1 @?= Nothing,
        testCase "tae Test 3" $ tae (Blatt "1") 1000000 @?= Nothing,

        testCase "tae Test 4" $ tae (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Knoten "9" 
              (Blatt "10")
              (Blatt "11")
            )
            (Knoten "12" 
              (Blatt "13")
              (Blatt "14")
            )
          )
        ) 1 @?= Just ["3", "8"],

        testCase "tae Test 5" $ tae (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Knoten "9" 
              (Blatt "10")
              (Blatt "11")
            )
            (Knoten "12" 
              (Blatt "13")
              (Blatt "14")
            )
          )
        ) 2 @?= Just ["4", "7", "9", "12"],

        testCase "tae Test 6" $ tae (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Knoten "9" 
              (Blatt "10")
              (Blatt "11")
            )
            (Knoten "12" 
              (Blatt "13")
              (Blatt "14")
            )
          )
        ) 3 @?= Just ["5", "6", "10", "11", "13", "14"],

        testCase "tae Test 7" $ tae (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Blatt "5")
              (Blatt "6")
            )
            (Blatt "7")
          )
          (Knoten "8" 
            (Knoten "9" 
              (Blatt "10")
              (Blatt "11")
            )
            (Knoten "12" 
              (Blatt "13")
              (Blatt "14")
            )
          )
        ) 4 @?= Nothing,

        testCase "tae Test 8" $ tae (
        Knoten "1"
          (Knoten "3" 
            (Knoten "4" 
              (Knoten "5" 
                (Knoten "6" 
                  (Knoten "7" 
                    (Knoten "8" 
                      (Blatt "9")
                      (Blatt "10")
                    )
                    (Blatt "11")
                  )
                  (Blatt "12")
                )
                (Blatt "13")
              )
              (Blatt "14")
            )
            (Blatt "15")
          )
          (Blatt "16")
        ) 5 @?= Just ["7", "12"],


        --Aufgabe 3
        testCase "awi Test 0" $ awi (TB) @?= TB' [],
        testCase "awi Test 1" $ awi (TK TB TB TB) @?= TK' [] (TB' [L]) (TB' [M]) (TB' [R]),
        testCase "awi Test 2" $ awi (TK (TK TB (TK TB (TK TB (TK TB TB TB) TB) TB) TB) (TK TB (TK TB TB TB) TB) TB) @?= TK' [] (TK' [L] (TB' [L,L]) (TK' [L,M] (TB' [L,M,L]) (TK' [L,M,M] (TB' [L,M,M,L]) (TK' [L,M,M,M] (TB' [L,M,M,M,L]) (TB' [L,M,M,M,M]) (TB' [L,M,M,M,R])) (TB' [L,M,M,R])) (TB' [L,M,R])) (TB' [L,R])) (TK' [M] (TB' [M,L]) (TK' [M,M] (TB' [M,M,L]) (TB' [M,M,M]) (TB' [M,M,R])) (TB' [M,R])) (TB' [R]),
        testCase "awi Test 3" $ awi (TK TB (TK TB (TK TB TB TB) (TK TB (TK TB TB TB) TB)) TB) @?= TK' [] (TB' [L]) (TK' [M] (TB' [M,L]) (TK' [M,M] (TB' [M,M,L]) (TB' [M,M,M]) (TB' [M,M,R])) (TK' [M,R] (TB' [M,R,L]) (TK' [M,R,M] (TB' [M,R,M,L]) (TB' [M,R,M,M]) (TB' [M,R,M,R])) (TB' [M,R,R]))) (TB' [R]),
        testCase "awi Test 4" $ awi (TK (TK TB TB TB) TB (TK (TK TB TB TB) TB TB)) @?= TK' [] (TK' [L] (TB' [L,L]) (TB' [L,M]) (TB' [L,R])) (TB' [M]) (TK' [R] (TK' [R,L] (TB' [R,L,L]) (TB' [R,L,M]) (TB' [R,L,R])) (TB' [R,M]) (TB' [R,R])),
        testCase "awi Test 5" $ awi (TK (TK (TK (TK (TK TB TB TB) (TK (TK TB TB TB) TB (TK TB (TK TB TB TB) TB)) TB) TB (TK TB (TK TB TB (TK TB TB TB))TB)) TB (TK TB TB (TK TB TB TB))) TB TB) @?= TK' [] (TK' [L] (TK' [L,L] (TK' [L,L,L] (TK' [L,L,L,L] (TB' [L,L,L,L,L]) (TB' [L,L,L,L,M]) (TB' [L,L,L,L,R])) (TK' [L,L,L,M] (TK' [L,L,L,M,L] (TB' [L,L,L,M,L,L]) (TB' [L,L,L,M,L,M]) (TB' [L,L,L,M,L,R])) (TB' [L,L,L,M,M]) (TK' [L,L,L,M,R] (TB' [L,L,L,M,R,L]) (TK' [L,L,L,M,R,M] (TB' [L,L,L,M,R,M,L]) (TB' [L,L,L,M,R,M,M]) (TB' [L,L,L,M,R,M,R])) (TB' [L,L,L,M,R,R]))) (TB' [L,L,L,R])) (TB' [L,L,M]) (TK' [L,L,R] (TB' [L,L,R,L]) (TK' [L,L,R,M] (TB' [L,L,R,M,L]) (TB' [L,L,R,M,M]) (TK' [L,L,R,M,R] (TB' [L,L,R,M,R,L]) (TB' [L,L,R,M,R,M]) (TB' [L,L,R,M,R,R]))) (TB' [L,L,R,R]))) (TB' [L,M]) (TK' [L,R] (TB' [L,R,L]) (TB' [L,R,M]) (TK' [L,R,R] (TB' [L,R,R,L]) (TB' [L,R,R,M]) (TB' [L,R,R,R])))) (TB' [M]) (TB' [R]),

        --Aufgabe 4
        testCase "show Test 1" $ show (B ([] :: [Int])) @?= "<[]>",
        testCase "show Test 2" $ show (B [1, 2, 3]) @?= "<[1,2,3]>",
        testCase "show Test 3" $ show (B [True, False, True]) @?= "<[True,False,True]>",
        testCase "show Test 4" $ show (B ["Functional", "Programming", "is", "Fun"]) @?= "<[\"Functional\",\"Programming\",\"is\",\"Fun\"]>",
        testCase "show Test 5" $ show (K (B []) [1, 2, 3] (B [4, 5, 6])) @?= "<Wurzel [1,2,3] <[]> <[4,5,6]>>",
        testCase "show Test 6" $ show (K (K (B []) [1, 2, 3] (B [4, 5, 6])) [] (B [7, 8, 9])) @?= "<Wurzel [] <Wurzel [1,2,3] <[]> <[4,5,6]>> <[7,8,9]>>",
        testCase "Eq Test 1" $ (B "bier") == (B "bier") @?= True,
        testCase "Eq Test 2" $ (B "Knopers") == (B []) @?= False,
        testCase "Eq Test 3" $ (K (B "bier") "bier" (B "bier")) == (B "bier") @?= False,
        testCase "Eq Test 4" $ (B "bier") == (K (B "bier") "bier" (B "bier")) @?= False,
        testCase "Eq Test 5" $ (K (B "b") "a" (B "c")) == (K (B "b") "a" (B "c")) @?= True,
        testCase "Eq Test 6" $ (K (B "b") "x" (B "c")) == (K (B "b") "a" (B "c")) @?= False,
        testCase "Eq Test 7" $ (K (B "x") "a" (B "c")) == (K (B "b") "a" (B "c")) @?= False,
        testCase "Eq Test 8" $ (K (B "b") "a" (K (B "d") "c" (B "e"))) == (K (B "b") "a" (B "c")) @?= False,
        
        --Aufgabe 5
        testCase "intervall Test 1" $ show (B [IV (2, 5)]) @?= "<[<2,5>]>",
        testCase "intervall Test 2" $ show (B [IV (5, 2)]) @?= "<[<>]>",
        testCase "intervall Test 3" $ show (B [Leer]) @?= "<[<>]>",
        testCase "intervall Test 4" $ show (B [IV (2, 5), IV (5, 2), Leer, Ungueltig]) @?= "<[<2,5>,<>,<>,Kein Intervall]>",
        testCase "intervall Test 5" $ show (K (B [IV (2, 2)]) [IV (2, 3)] (B [Leer])) @?= "<Wurzel [<2,3>] <[<2,2>]> <[<>]>>"

    ]
