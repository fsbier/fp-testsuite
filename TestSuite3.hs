module TestSuite3 where

import Test.Tasty
import Test.Tasty.HUnit
import Angabe3

import Control.Exception (ErrorCall(ErrorCallWithLocation), try, evaluate)

getMsg :: ErrorCall -> String
getMsg (ErrorCallWithLocation msg _) = msg

assertError :: (Show a) => String -> a -> IO ()
assertError errorMsg action = do
    r <- try (evaluate action)
    case r of
      Left e -> if (getMsg e == errorMsg) then return()
                else assertFailure $ "Received unexpected error: " ++ (show e) ++ "\ninstead of error: " ++ errorMsg
      Right _ -> assertFailure $ "Expected error: " ++ errorMsg

spec :: TestTree
spec =
  testGroup
    "Angabe3 Tests"
    [
      testCase "show Ungültig" $ show Ungueltig @?= "Kein Intervall",
      testCase "show Leer" $ show Leer @?= "<>",
      testCase "show <1,1>" $ show (IV (1,1)) @?= "<1,1>",
      testCase "show <5,2>" $ show (IV (5,2)) @?= "<>",
      testCase "show <2,5>" $ show (IV (2,5)) @?= "<2,5>",

      testCase "Eq ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig == Leer),
      testCase "Eq ungueltig" $ assertError "Vergleich nicht moeglich" (Leer == Ungueltig),
      testCase "Eq ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig == Ungueltig),
      testCase "Eq Leer 1" $ Leer == Leer @?= True,
      testCase "nEq Leer 2" $ Leer /= Leer @?= False,
      testCase "Eq Leer 3" $ Leer == (IV (3,5)) @?= False,
      testCase "Eq Leer 4" $ (IV (3,5)) == Leer @?= False,
      testCase "Eq Leer 5" $ Leer == (IV (5,3)) @?= True,
      testCase "Eq Leer 6" $ (IV (6,2)) == (IV (5,3)) @?= True,
      --testCase "Eq ID 1" $ 5 == (IV (5,5)) @?= True,
      testCase "Eq ID 2" $ (IV (5,5)) == (IV (5,5)) @?= True,
      --testCase "Eq ID 3" $ 5 == (IV (3,5)) @?= False,
      testCase "Eq Range 1" $ (IV (3,5)) == (IV (3,5)) @?= True,
      testCase "Eq Range 2" $ (IV (1,5)) == (IV (3,5)) @?= False,

      testCase "Ord 00" $ assertError "Vergleich nicht moeglich" (compare Ungueltig Leer),
      testCase "Ord 01" $ assertError "Vergleich nicht moeglich" (compare Leer Ungueltig),
      testCase "Ord 02" $ assertError "Vergleich nicht moeglich" (compare Ungueltig Ungueltig),
      testCase "Ord 1" $ Leer < (IV(1,3)) @?= True,
      testCase "Ord 2" $ IV (1,3) < Leer @?= False,
      testCase "Ord 3" $ (IV(1,1)) < (IV(1,1)) @?= False,
      testCase "Ord 4" $ (IV(1,1)) <= (IV(1,1)) @?= True,
      testCase "Ord 5" $ (IV(1,1)) >= (IV(1,1)) @?= True,
      testCase "Ord 6" $ (IV(1,1)) > (IV(1,1)) @?= False,
      testCase "Ord 7" $ IV (1,3) >= IV (1,3) @?= True,
      testCase "Ord 8" $ IV (1,3) > IV (1,3) @?= False,
      testCase "Ord 9" $ IV (3,4) < IV (3,5) @?= True,
      testCase "Ord 10" $ IV (3,4) <= IV (3,5) @?= True,
      testCase "Ord 11" $ IV (3,4) >= IV (3,5) @?= False,
      testCase "Ord 12" $ IV (4,5) < IV (3,5) @?= True,
      testCase "Ord 13" $ IV (4,5) <= IV (3,5) @?= True,
      testCase "Ord 14" $ IV (4,5) >= IV (3,5) @?= False,
      testCase "Ord 15" $ IV (3,5) > IV (4,5) @?= True,
      testCase "Ord 16" $ IV (3,5) >= IV (4,5) @?= True,
      testCase "Ord 17" $ IV (3,5) <= IV (4,5) @?= False,
      testCase "Ord 18" $ IV (3,5) < IV (3,4) @?= False,
      testCase "Ord 19" $ IV (3,5) > IV (3,4) @?= True,
      testCase "Ord 20" $ IV (3,5) > IV (4,6) @?= False,
      testCase "Ord 21" $ IV (3,5) < IV (4,6) @?= False,
      testCase "Ord 22" $ IV (3,5) <= IV (4,6) @?= False,
      testCase "Ord 23" $ IV (3,5) >= IV (4,6) @?= False,

      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig + Ungueltig),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig + Leer),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Leer + Ungueltig),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig * Ungueltig),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig * Leer),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Leer * Ungueltig),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig - Ungueltig),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Ungueltig - Leer),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (Leer - Ungueltig),
      testCase "Num ungueltig" $ assertError "Vergleich nicht moeglich" (abs Ungueltig),
      testCase "Num plus" $ assertError "Vergleich nicht moeglich" (Ungueltig + Ungueltig),
      testCase "Num plus" $ assertError "Vergleich nicht moeglich" (Ungueltig + Leer),
      testCase "Num plus" $ assertError "Vergleich nicht moeglich" (Leer + Ungueltig),
      testCase "Num plus" $ Leer + Leer @?= Leer,
      testCase "Num plus" $ Leer + (IV(2,5)) @?= Leer,
      testCase "Num plus" $ (IV(2,5)) + Leer @?= Leer,
      testCase "Num plus" $ (IV(2,5)) + (IV(5,2)) @?= Leer,
      testCase "Num plus" $ (IV(5,2)) + (IV(2,5)) @?= Leer,
      testCase "Num plus" $ IV (2,5) + IV (-3,-2) @?= IV (-1,3),
      testCase "Num plus" $ IV (2,5) + IV (1,1) @?= IV (3,6),
      testCase "Num plus" $ IV (2,5) + IV (1,2) @?= IV (3,7),
      testCase "Num plus" $ IV (2,5) + IV (-1,1) @?= IV (1,6),
      testCase "Num minus" $ assertError "Vergleich nicht moeglich" (Ungueltig - Ungueltig),
      testCase "Num minus" $ assertError "Vergleich nicht moeglich" (Ungueltig - Leer),
      testCase "Num minus" $ assertError "Vergleich nicht moeglich" (Leer - Ungueltig),
      testCase "Num minus" $ Leer - Leer @?= Leer,
      testCase "Num minus" $ Leer - IV (2,5) @?= Leer,
      testCase "Num minus" $ IV (2,5) - Leer @?= Leer,
      testCase "Num minus" $ IV (2,5) - IV (5,2) @?= Leer,
      testCase "Num minus" $ IV (5,2) - IV (2,5) @?= Leer,
      testCase "Num minus" $ IV (2,5) - IV (2,3) @?= IV (-1,3),
      testCase "Num minus" $ IV (2,5) - IV (-3,-2) @?= IV (4,8),
      testCase "Num minus" $ IV (-5,-2) - IV (-3,-2) @?= IV (-3,1),
      testCase "Num mal" $ assertError "Vergleich nicht moeglich" (Ungueltig * Ungueltig),
      testCase "Num mal" $ assertError "Vergleich nicht moeglich" (Ungueltig * Leer),
      testCase "Num mal" $ assertError "Vergleich nicht moeglich" (Leer * Ungueltig),
      testCase "Num mal" $ Leer * Leer @?= Leer,
      testCase "Num mal" $ Leer * IV (-3,-2) @?= Leer,
      testCase "Num mal" $ IV (-3,-2) * Leer @?= Leer,
      testCase "Num mal" $ IV (2,5) * IV (5,2) @?= Leer,
      testCase "Num mal" $ IV (5,2) * IV (2,5) @?= Leer,
      testCase "Num mal" $ IV (2,5) * IV (-3,-2) @?= IV (-15,-4),
      testCase "Num mal" $ IV (2,3) * IV (2,-3) @?= Leer,
      testCase "Num mal" $ IV (2,3) * IV (2,4) @?= IV (4,12),
      testCase "Num abs 1" $ abs (IV (1,2)) @?= IV (1,2),
      testCase "Num abs 2" $ abs (IV (-2,-1)) @?= IV (1,2),
      testCase "Num abs 3" $ abs (IV (-1,2)) @?= IV (0,2),
      testCase "Num abs 4" $ abs (IV (2,1)) @?= Leer,

      testCase "Enum ungueltig" $ assertError "Operation nicht moeglich" $ fromEnum Ungueltig,
      testCase "Enum leer" $ assertError "Operation nicht moeglich" $ fromEnum Leer,
      testCase "Enum range" $ assertError "Operation nicht moeglich" $ fromEnum (IV (1,2)),
      testCase "Enum singular" $ fromEnum (IV (1,1)) @?= 1,
      testCase "Enum singular" $ fromEnum (IV (-1,-1)) @?= -1,
      testCase "Enum retour" $ (toEnum 1 :: Intervall) @?= IV (1,1),

      testCase "Kanonisch ungueltig" $ show (kanonisch Ungueltig) @?= show Ungueltig,
      testCase "Kanonisch leer" $ kanonisch Leer @?= Leer,
      testCase "Kanonisch leer" $ kanonisch (IV (1,0)) @?= Leer,
      testCase "Kanonisch id" $ kanonisch (IV (1,1)) @?= IV (1,1),
      testCase "Kanonisch range" $ kanonisch (IV (1,2)) @?= IV (1,2),

      testCase "Element ungueltig" $ is_elem 1 Ungueltig @?= Nothing,
      testCase "Element leer" $ is_elem 1 Leer @?= Just False,
      testCase "Element leer" $ is_elem 1 (IV (2,1)) @?= Just False,
      testCase "Element 0" $ is_elem 0 (IV (1,3)) @?= Just False,
      testCase "Element 1" $ is_elem 1 (IV (1,3)) @?= Just True,
      testCase "Element 2" $ is_elem 2 (IV (1,3)) @?= Just True,
      testCase "Element 3" $ is_elem 3 (IV (1,3)) @?= Just True,
      testCase "Element 4" $ is_elem 4 (IV (1,3)) @?= Just False,

      testCase "Codiere leer" $ codiere [] @?= Leer,
      testCase "Codiere 1" $ codiere [2,3,4,5] @?= IV (2,5),
      testCase "Codiere 2" $ codiere [2..5] @?= IV (2,5),
      testCase "Codiere lücke" $ show (codiere [2,4,5] :: Intervall) @?= show Ungueltig,
      testCase "Codiere nicht geordnet, lücke" $ show (codiere [2,5,4] :: Intervall) @?= show Ungueltig,
      testCase "Codiere nicht geordnet" $ show (codiere [3,2,5,4] :: Intervall) @?= show Ungueltig,
      testCase "Codiere Duplicat" $ show (codiere [2,2,3,4,5] :: Intervall) @?= show Ungueltig,
      testCase "Codiere rückwärts" $ show (codiere [5,4..2] :: Intervall) @?= show Ungueltig,
      testCase "Codiere ID" $ (codiere [1] :: Intervall) @?= IV (1,1),

      testCase "Decodiere ungueltig" $ decodiere Ungueltig @?= Nothing,
      testCase "Decodiere leer" $ decodiere Leer @?= Just [],
      testCase "Decodiere id" $ decodiere (IV (1,1)) @?= Just [1],
      testCase "Decodiere range" $ decodiere (IV (2,5)) @?= Just [2,3,4,5],
      testCase "Decodiere leer" $ decodiere (IV (5,2)) @?= Just [],

      testCase "extrahiere nothing" $ assertError "Extraktion nicht moeglich" (extrahiere Nothing :: [Int]),
      testCase "extrahiere leer" $ extrahiere (Just [] :: Maybe [Int]) @?= [],
      testCase "extrahiere leer" $ extrahiere (Just [5..2] :: Maybe [Int]) @?= [],
      testCase "extrahiere leer" $ extrahiere (Just [5,4..2] :: Maybe [Int]) @?= [5,4,3,2],
      testCase "extrahiere id" $ extrahiere (Just [2..2] :: Maybe [Int]) @?= [2],
      testCase "extrahiere 1" $ extrahiere (Just [2..5] :: Maybe [Int]) @?= [2,3,4,5],

      testCase "aufsteigend leer" $ ist_aufsteigend ([]::[Int]) @?= True,
      testCase "aufsteigend id" $ ist_aufsteigend ([1]::[Int]) @?= True,
      testCase "aufsteigend 1" $ ist_aufsteigend ([2..5]::[Int]) @?= True,
      testCase "aufsteigend 2" $ ist_aufsteigend ([2,5..21]::[Int]) @?= True,
      testCase "aufsteigend 3" $ ist_aufsteigend ([5,4..2]::[Int]) @?= False,

      testCase "lueckenlos leer" $ ist_lueckenlos ([]::[Int]) @?= True,
      testCase "lueckenlos id" $ ist_lueckenlos ([1]::[Int]) @?= True,
      testCase "lueckenlos 1" $ ist_lueckenlos ([1,2]::[Int]) @?= True,
      testCase "lueckenlos 2" $ ist_lueckenlos ([1,3]::[Int]) @?= False,
      testCase "lueckenlos 3" $ ist_lueckenlos ([2..5]::[Int]) @?= True,
      testCase "lueckenlos 4" $ ist_lueckenlos ([2,5..21]::[Int]) @?= False,
      testCase "lueckenlos 5" $ ist_lueckenlos ([5,4..2]::[Int]) @?= True,
      testCase "lueckenlos 6" $ ist_lueckenlos ([5,3,4,1,2]::[Int]) @?= True,
      testCase "lueckenlos 7" $ ist_lueckenlos ([5,3,4,5,1,3,5,2]::[Int]) @?= True,

      testCase "laL Element nothing" $ ist_laL_Element (3::Int) Nothing @?= False,
      testCase "laL Element leer" $ ist_laL_Element (3::Int) (Just []) @?= False,
      testCase "laL Element id 1" $ ist_laL_Element (3::Int) (Just [3]) @?= True,
      testCase "laL Element id 2" $ ist_laL_Element (3::Int) (Just [4]) @?= False,
      testCase "laL Element 1" $ ist_laL_Element (3::Int) (Just [2..5]) @?= True,
      testCase "laL Element 2" $ ist_laL_Element (2::Int) (Just [2..5]) @?= True,
      testCase "laL Element 3" $ ist_laL_Element (3::Int) (Just [2,3,5]) @?= False,
      testCase "laL Element 4" $ ist_laL_Element (3::Int) (Just [5,4..2]) @?= False,
      testCase "laL Element 5" $ ist_laL_Element (8::Int) (Just [2,5..21]) @?= False,
      testCase "laL Element 6" $ ist_laL_Element (5::Int) (Just [1,2,3,3,4,5,5,5]) @?= True
    ]
