module TestSuite2 where

import Test.Tasty
import Test.Tasty.HUnit
import Angabe2

spec :: TestTree
spec =
  testGroup
    "Angabe2 Tests"
    [
      testCase "IstEinsBinaerPrim 0" $ ist_einsbp 0 @?= Nein,
      testCase "IstEinsBinaerPrim 1" $ ist_einsbp 1 @?= Nein,
      testCase "IstEinsBinaerPrim 2" $ ist_einsbp 2 @?= Nein,
      testCase "IstEinsBinaerPrim 3" $ ist_einsbp 3 @?= Ja,
      testCase "IstEinsBinaerPrim 4" $ ist_einsbp 4 @?= Nein,
      testCase "IstEinsBinaerPrim 5" $ ist_einsbp 5 @?= Ja,
      testCase "IstEinsBinaerPrim 6" $ ist_einsbp 6 @?= Ja,
      testCase "IstEinsBinaerPrim 7" $ ist_einsbp 7 @?= Ja,
      testCase "IstEinsBinaerPrim 8" $ ist_einsbp 8 @?= Nein,
      testCase "IstEinsBinaerPrim 9" $ ist_einsbp 9 @?= Ja,
      testCase "IstEinsBinaerPrim 26165" $ ist_einsbp 26165 @?= Nein,

      testCase "Anzahlen01bps (0,0)" $ anz01bps (0,0) @?= (0,0),
      testCase "Anzahlen01bps (0,5)" $ anz01bps (0,5) @?= (1,2),
      testCase "Anzahlen01bps (5,5)" $ anz01bps (5,5) @?= (0,1),
      testCase "Anzahlen01bps (5,0)" $ anz01bps (5,0) @?= (-1,-1),
      testCase "Anzahlen01bps (0,100)" $ anz01bps (0,100) @?= (58,65),

      testCase "Wortliste 0.0" $ liefere_woerter_in "" @?=[],
      testCase "Wortliste 0.1" $ liefere_woerter_in " " @?=[],
      testCase "Wortliste 0.2" $ liefere_woerter_in "\n \t" @?=[],
      testCase "Wortliste 1" $
        liefere_woerter_in "Functional Programming is Fun" @?=
          ["Functional","Programming","is","Fun"],
      testCase "Wortliste 2" $
        liefere_woerter_in "Functional Programming is Fun, isn't it?" @?=
          ["Functional","Programming","is","Fun,","isn't","it?"],
      testCase "Wortliste 3" $
        liefere_woerter_in " Complex\tBounds will,\nundoubtedly, bring \n Desaster.\n" @?=
          ["Complex","Bounds","will,","undoubtedly,","bring","Desaster."],

      testCase "Hammingabstand 0.0" $ hM [] @?= -1,
      testCase "Hammingabstand 0.1" $ hM ["test"] @?= -1,
      testCase "Hammingabstand 0.2" $ hM ["test Zero", "test Zero", "test Zero"] @?= 0,
      testCase "Hammingabstand 1" $ hM ["Fahrrad","Autobus"] @?= 7,
      testCase "Hammingabstand 2" $ hM ["1001","1111","1100"] @?= 2,
      testCase "Hammingabstand 3" $ hM ["Haskell","Fortran","Miranda","Clojure"] @?= 6,
      testCase "Hammingabstand 4" $ hM ["Haskell","Java","Prolog"] @?= -1,
      testCase "Hammingabstand 5" $ hM ["test 01", "test 11", "test XX"] @?= 1
    ]
