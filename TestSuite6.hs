module TestSuite6 where

import Angabe6
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

datenbank1 = [
  P 1 "Elsie" 58 F 5784 (Just Motorola),
  P 2 "Jay" 26 M 224 (Just Samsung),
  P 3 "Niamh" 37 D 635 (Just Huawai),
  P 4 "Cora" 22 F 742 (Just Samsung),
  P 5 "Kimberley" 7 M 3587 Nothing,
  P 6 "Esther" 98 M 7052 (Just LG),
  P 7 "Gemma" 78 M 6578 (Just Apple),
  P 8 "Demi" 55 M 2790 (Just Huawai),
  P 9 "Iqra" 54 M 6510 (Just Apple)
  ]

spec :: TestTree
spec =
  testGroup
    "Angabe6 Tests"
    [ gen_sort_tests,
      gen_insert_tests,
      auf_ord_tests,
      ab_ord_tests,
      auf_lst_tests,
      ab_lst_tests,
      database_test
    ]

gen_sort_tests :: TestTree
gen_sort_tests =
  testGroup
    "A.1 gen_sort"
    [ testCase "A1 01" $ gen_sort (<=) ([] :: [Int]) @?= [],
      testCase "A1 02" $ gen_sort (<=) [3, 5, 2, 4] @?= [2, 3, 4, 5],
      testCase "A1 03" $ gen_sort (<=) [3, 5, 3, 4, 2, 4] @?= [2, 3, 3, 4, 4, 5],
      testCase "A1 04" $ gen_sort (<) ([] :: [Int]) @?= [],
      testCase "A1 05" $ gen_sort (<) [3, 5, 2, 4] @?= [2, 3, 4, 5],
      testCase "A1 06" $ gen_sort (<) [3, 5, 3, 4, 2, 4] @?= [2, 3, 3, 4, 4, 5],
      testCase "A1 07" $ gen_sort (>=) ([] :: [Int]) @?= [],
      testCase "A1 08" $ gen_sort (>=) [3, 5, 2, 4] @?= [5, 4, 3, 2],
      testCase "A1 09" $ gen_sort (>=) [3, 5, 3, 4, 2, 4] @?= [5, 4, 4, 3, 3, 2],
      testCase "A1 10" $ gen_sort (>) ([] :: [Int]) @?= [],
      testCase "A1 11" $ gen_sort (>) [3, 5, 2, 4] @?= [5, 4, 3, 2],
      testCase "A1 12" $ gen_sort (>=) [3, 5, 3, 4, 2, 4] @?= [5, 4, 4, 3, 3, 2]
    ]

gen_insert_tests :: TestTree
gen_insert_tests =
  testGroup
    "A.1 gen_insert"
    [ testCase "A1 13" $ gen_insert (<=) 4 [] @?= [4],
      testCase "A1 14" $ gen_insert (<=) 4 [1, 3, 5, 7, 9] @?= [1, 3, 4, 5, 7, 9],
      testCase "A1 15" $ gen_insert (<=) 4 [1, 3, 4, 4, 5, 7, 9] @?= [1, 3, 4, 4, 4, 5, 7, 9],
      testCase "A1 16" $ gen_insert (<=) 4 [1, 3, 1, 3, 6, 8] @?= [1, 3, 1, 3, 4, 6, 8],
      testCase "A1 17" $ gen_insert (<) 4 [] @?= [4],
      testCase "A1 18" $ gen_insert (<) 4 [1, 3, 5, 7, 9] @?= [1, 3, 4, 5, 7, 9],
      testCase "A1 19" $ gen_insert (<) 4 [1, 3, 4, 4, 5, 7, 9] @?= [1, 3, 4, 4, 4, 5, 7, 9],
      testCase "A1 20" $ gen_insert (<) 4 [1, 3, 1, 3, 6, 8] @?= [1, 3, 1, 3, 4, 6, 8],
      testCase "A1 21" $ gen_insert (>=) 4 [] @?= [4],
      testCase "A1 22" $ gen_insert (>=) 4 [9, 7, 5, 3, 1] @?= [9, 7, 5, 4, 3, 1],
      testCase "A1 23" $ gen_insert (>=) 4 [1, 3, 4, 4, 5, 7, 9] @?= [4, 1, 3, 4, 4, 5, 7, 9],
      testCase "A1 24" $ gen_insert (>=) 4 [1, 3, 1, 3, 6, 8] @?= [4, 1, 3, 1, 3, 6, 8],
      testCase "A1 25" $ gen_insert (>=) 4 [10, 13, 4, 4, 5, 7, 9] @?= [10, 13, 4, 4, 4, 5, 7, 9],
      testCase "A1 25" $ gen_insert (>) 4 [] @?= [4],
      testCase "A1 26" $ gen_insert (>) 4 [9, 7, 5, 3, 1] @?= [9, 7, 5, 4, 3, 1],
      testCase "A1 27" $ gen_insert (>) 4 [1, 3, 4, 4, 5, 7, 9] @?= [4, 1, 3, 4, 4, 5, 7, 9],
      testCase "A1 28" $ gen_insert (>) 4 [1, 3, 1, 3, 6, 8] @?= [4, 1, 3, 1, 3, 6, 8],
      testCase "A1 29" $ gen_insert (>) 4 [10, 13, 4, 4, 5, 7, 9] @?= [10, 13, 4, 4, 5, 7, 9, 4]
    ]

auf_ord_tests :: TestTree
auf_ord_tests =
  testGroup
    "A.3 auf_ord"
    [ testCase "A3 01" $ auf_ord ([] :: [Int]) @?= [],
      testCase "A3 02" $ auf_ord [3, 5, 2, 4] @?= [2, 3, 4, 5],
      testCase "A3 03" $ auf_ord [3, 5, 3, 4, 2, 4] @?= [2, 3, 3, 4, 4, 5],
      testCase "A3 04" $ auf_ord [3, 5, 2, 4] @?= [2, 3, 4, 5],
      testCase "A3 05" $ auf_ord [3, 5, 3, 4, 2, 4] @?= [2, 3, 3, 4, 4, 5]
    ]

ab_ord_tests :: TestTree
ab_ord_tests =
  testGroup
    "A.3 ab_ord"
    [ testCase "A3 06" $ ab_ord ([] :: [Int]) @?= [],
      testCase "A3 07" $ ab_ord [3, 5, 2, 4] @?= [5, 4, 3, 2],
      testCase "A3 08" $ ab_ord [3, 5, 3, 4, 2, 4] @?= [5, 4, 4, 3, 3, 2],
      testCase "A3 09" $ ab_ord [3, 5, 2, 4] @?= [5, 4, 3, 2],
      testCase "A3 10" $ ab_ord [3, 5, 3, 4, 2, 4] @?= [5, 4, 4, 3, 3, 2]
    ]

auf_lst_tests :: TestTree
auf_lst_tests =
  testGroup
    "A.8 auf_lst"
    [ testCase "A8 01" $ auf_lst ([] :: [[Int]]) @?= [],
      testCase "A8 02" $ auf_lst [[1 .. 4], [], [2, 1]] @?= [[], [2, 1], [1 .. 4]],
      testCase "A8 03" $ auf_lst [[], [1 .. 5], [], [2, 3, 1]] @?= [[], [], [2, 3, 1], [1 .. 5]],
      testCase "A8 04" $ auf_lst [[1 .. 5], [1], [1 .. 3], [1 .. 4], [1, 2]] @?= [[1], [1, 2], [1 .. 3], [1 .. 4], [1 .. 5]]
    ]

ab_lst_tests :: TestTree
ab_lst_tests =
  testGroup
    "A.8 ab_lst"
    [ testCase "A8 05" $ ab_lst ([] :: [[Int]]) @?= [],
      testCase "A8 06" $ ab_lst [[1 .. 4], [], [2, 1]] @?= [[1 .. 4], [2, 1], []],
      testCase "A8 07" $ ab_lst [[], [1 .. 5], [], [2, 3, 1]] @?= [[1 .. 5], [2, 3, 1], [], []],
      testCase "A8 08" $ ab_lst [[1 .. 5], [1], [1 .. 3], [1 .. 4], [1, 2]] @?= [[1 .. 5], [1 .. 4], [1 .. 3], [1, 2], [1]]
    ]


database_test :: TestTree
database_test =
  testGroup
    "A.13 Datenbank Personen"
    [ 
      testCase "A13 01" $ normalsicht datenbank1 @?= [P 4 "Cora" 22 F 742 (Just Samsung),P 8 "Demi" 55 M 2790 (Just Huawai),P 1 "Elsie" 58 F 5784 (Just Motorola),P 6 "Esther" 98 M 7052 (Just LG),P 7 "Gemma" 78 M 6578 (Just Apple),P 9 "Iqra" 54 M 6510 (Just Apple),P 2 "Jay" 26 M 224 (Just Samsung),P 5 "Kimberley" 7 M 3587 Nothing,P 3 "Niamh" 37 D 635 (Just Huawai)],
      testCase "A13 02" $ anlageberatungssicht datenbank1 @?= [P 6 "Esther" 98 M 7052 (Just LG),P 7 "Gemma" 78 M 6578 (Just Apple),P 9 "Iqra" 54 M 6510 (Just Apple),P 1 "Elsie" 58 F 5784 (Just Motorola),P 5 "Kimberley" 7 M 3587 Nothing,P 8 "Demi" 55 M 2790 (Just Huawai),P 4 "Cora" 22 F 742 (Just Samsung),P 3 "Niamh" 37 D 635 (Just Huawai),P 2 "Jay" 26 M 224 (Just Samsung)],
      testCase "A13 03" $ personalabteilungssicht datenbank1 @?= [P 3 "Niamh" 37 D 635 (Just Huawai),P 4 "Cora" 22 F 742 (Just Samsung),P 1 "Elsie" 58 F 5784 (Just Motorola),P 5 "Kimberley" 7 M 3587 Nothing,P 2 "Jay" 26 M 224 (Just Samsung),P 9 "Iqra" 54 M 6510 (Just Apple),P 8 "Demi" 55 M 2790 (Just Huawai),P 7 "Gemma" 78 M 6578 (Just Apple),P 6 "Esther" 98 M 7052 (Just LG)],
      testCase "A13 04" $ sozialforschungssicht datenbank1 @?= [P 7 "Gemma" 78 M 6578 (Just Apple),P 9 "Iqra" 54 M 6510 (Just Apple),P 8 "Demi" 55 M 2790 (Just Huawai),P 3 "Niamh" 37 D 635 (Just Huawai),P 6 "Esther" 98 M 7052 (Just LG),P 1 "Elsie" 58 F 5784 (Just Motorola),P 4 "Cora" 22 F 742 (Just Samsung),P 2 "Jay" 26 M 224 (Just Samsung),P 5 "Kimberley" 7 M 3587 Nothing],
      testCase "A13 05" $ integritaetssicht datenbank1 @?= [P 1 "Elsie" 58 F 5784 (Just Motorola),P 2 "Jay" 26 M 224 (Just Samsung),P 3 "Niamh" 37 D 635 (Just Huawai),P 4 "Cora" 22 F 742 (Just Samsung),P 5 "Kimberley" 7 M 3587 Nothing,P 6 "Esther" 98 M 7052 (Just LG),P 7 "Gemma" 78 M 6578 (Just Apple),P 8 "Demi" 55 M 2790 (Just Huawai),P 9 "Iqra" 54 M 6510 (Just Apple)],
      testCase "A13 06" $ auch_im_chaos_ist_ordnung_sicht datenbank1 @?= [P 4 "Cora" 22 F 742 (Just Samsung),P 8 "Demi" 55 M 2790 (Just Huawai),P 1 "Elsie" 58 F 5784 (Just Motorola),P 6 "Esther" 98 M 7052 (Just LG),P 7 "Gemma" 78 M 6578 (Just Apple),P 9 "Iqra" 54 M 6510 (Just Apple),P 2 "Jay" 26 M 224 (Just Samsung),P 5 "Kimberley" 7 M 3587 Nothing,P 3 "Niamh" 37 D 635 (Just Huawai)]
    ]

{-
p1 :: Int -> Int
p1 num = num + 1

p2 :: Int -> Int
p2 num = num + 2

p3 :: Int -> Int
p3 num = num + 3

auf_fun_tests :: TestTree
auf_fun_tests =
  testGroup
    "A.10 auf_fun"
    []
-}

--testCase "A10 01" $  auf_fun [p1] @?= [p1]
--testCase "A10 02" $  auf_fun [p3,p2,p1] @?= [p1,p2,p3],
--testCase "A10 03" $  auf_fun [p3,p1,p2] @?= [p1,p2,p3],
--testCase "A10 04" $  auf_fun [p1,p2,p3] @?= [p1,p2,p3]
