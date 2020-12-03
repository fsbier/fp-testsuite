module TestSuite7 where

import Angabe7 hiding (leer_band, leer_rb, showBZ, showRB)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

spec :: TestTree
spec =
  testGroup
    "Angabe7 Tests"
    [ akt_band_tests,
      akt_rechenband_test,
      wandle_in_rb_test,
      ist_zulaessige_Turingtafel_test,
      transition_test,
      spur_test,
      sim_test
    ]

akt_band_tests :: TestTree
akt_band_tests =
  testGroup
    "akt_band"
    [ testCase "1" $ akt_band (const Blank) 0 Blank 0 @?= Blank,
      testCase "2" $ akt_band (const Blank) 0 (Z '0') 0 @?= Z '0',
      testCase "3" $ akt_band (const (Z '0')) 0 (Z '1') 0 @?= Z '1',
      testCase "4" $ akt_band (Z . head . show) 1 (Z '0') 0 @?= Z '0',
      testCase "5" $ akt_band (Z . head . show) 1 (Z '0') 1 @?= Z '0',
      testCase "6" $ akt_band (Z . head . show) 1 (Z '0') 2 @?= Z '2'
    ]

akt_rechenband_test :: TestTree
akt_rechenband_test =
  testGroup
    "akt_rechenband"
    [ testCase "1" $ bi (akt_rechenband leer_rb 0 Blank) @?= "Leer",
      testCase "2" $ bi (akt_rechenband leer_rb 0 (Z 'X')) @?= "0>X<0",
      testCase "3" $ bi (stringToRB "Test Wort") @?= "1>Test Wort<9",
      testCase "4" $ bi (akt_rechenband (stringToRB "A") 2 (Z 'X')) @?= "1>AX<2",
      testCase "5" $ bi (akt_rechenband (stringToRB "A") 0 (Z 'X')) @?= "0>XA<1",
      testCase "6" $ bi (akt_rechenband (stringToRB "A") 1 Blank) @?= "Leer",
      testCase "7" $ bi (akt_rechenband (stringToRB "A") 2 Blank) @?= "1>A<1",
      testCase "8" $ bi (akt_rechenband (stringToRB "ABC") 1 Blank) @?= "2>BC<3",
      testCase "10" $ bi (akt_rechenband (stringToRB "ABC") 3 Blank) @?= "1>AB<2",
      testCase "12" $ bi (akt_rechenband (stringToRB "ABC") 0 (Z 'X')) @?= "0>XABC<3",
      testCase "13" $ bi (akt_rechenband (stringToRB "ABC") 1 (Z 'X')) @?= "1>XBC<3",
      testCase "14" $ bi (akt_rechenband (stringToRB "ABC") 2 (Z 'X')) @?= "1>AXC<3",
      testCase "15" $ bi (akt_rechenband (stringToRB "ABC") 3 (Z 'X')) @?= "1>ABX<3",
      testCase "16" $ bi (akt_rechenband (stringToRB "ABC") 4 (Z 'X')) @?= "1>ABCX<4",
      testCase "18" $ bi (akt_rechenband (akt_rechenband (stringToRB "ABC") 3 Blank) 2 Blank) @?= "1>A<1",
      testCase "19" $ bi (akt_rechenband (akt_rechenband (stringToRB "ABC") 1 Blank) 2 Blank) @?= "3>C<3"
    ]

wandle_in_rb_test :: TestTree
wandle_in_rb_test =
  testGroup
    "wandle_in_rb"
    [ testCase "1" $ bi (wandle_in_rb "") @?= "Leer",
      testCase "2" $ bi (wandle_in_rb "A") @?= "1>A<1",
      testCase "3" $ bi (wandle_in_rb "ABC") @?= "1>ABC<3",
      testCase "4" $ bi (wandle_in_rb "Test Wort") @?= "1>Test Wort<9"
    ]

ist_zulaessige_Turingtafel_test :: TestTree
ist_zulaessige_Turingtafel_test =
  testGroup
    "ist_zulaessige_Turingtafel"
    [ testCase "1" $ ist_zulaessige_Turingtafel [] @?= False,
      testCase "2" $ ist_zulaessige_Turingtafel tt @?= True,
      testCase "3" $ ist_zulaessige_Turingtafel (head tt : tt) @?= False
    ]

transition_test :: TestTree
transition_test =
  testGroup
    "transition"
    [ testCase "1" $ zeige_zustand (transition (GZ tt (stringToRB "()") 0 0)) @?= "(IZ:1,LSK:1,B:(),Min:1,Max:2)",
      testCase "2" $ zeige_zustand (transition (GZ tt leer_rb 0 0)) @?= "(IZ:1,LSK:1,B:unbeschrieben)",
      testCase "3" $ zeige_zustand (transition (GZ tt leer_rb 1 1)) @?= "(IZ:4,LSK:0,B:unbeschrieben)",
      testCase "4" $ zeige_zustand (transition (GZ tt leer_rb 4 0)) @?= "(IZ:5,LSK:-1,B:unbeschrieben)",
      testCase "6" $ zeige_zustand (transition (GZ tt leer_rb 5 0)) @?= "(IZ:5,LSK:0,B:unbeschrieben)",
      testCase "7" $ zeige_zustand (transition (GZ tt (stringToRB "()") 1 2)) @?= "(IZ:2,LSK:2,B:(x,Min:1,Max:2)",
      testCase "8" $ zeige_zustand (transition (GZ tt (stringToRB "(x") 2 2)) @?= "(IZ:2,LSK:1,B:(x,Min:1,Max:2)",
      testCase "9" $ zeige_zustand (transition (GZ tt (stringToRB "(x") 2 1)) @?= "(IZ:1,LSK:1,B:xx,Min:1,Max:2)"
    ]

spur_test :: TestTree
spur_test =
  testGroup
    "spur"
    [ testCase "1" $ zeige_spur (spur (GZ tt leer_rb 0 0)) @?=
        "(IZ:0,LSK:0,B:unbeschrieben) ->> (IZ:1,LSK:1,B:unbeschrieben) ->> (IZ:4,LSK:0,B:unbeschrieben) ->> (IZ:5,LSK:-1,B:unbeschrieben)",
      testCase "2" $ zeige_spur (spur (GZ tt (stringToRB "[]") 0 0)) @?=
        "(IZ:0,LSK:0,B:[],Min:1,Max:2) ->> (IZ:1,LSK:1,B:[],Min:1,Max:2) ->> (IZ:1,LSK:2,B:[],Min:1,Max:2) ->> (IZ:3,LSK:2,B:[x,Min:1,Max:2) ->> (IZ:3,LSK:1,B:[x,Min:1,Max:2) ->> (IZ:1,LSK:1,B:xx,Min:1,Max:2) ->> (IZ:1,LSK:2,B:xx,Min:1,Max:2) ->> (IZ:1,LSK:3,B:xx,Min:1,Max:2) ->> (IZ:4,LSK:2,B:xx,Min:1,Max:2) ->> (IZ:4,LSK:1,B:xx,Min:1,Max:2) ->> (IZ:4,LSK:0,B:xx,Min:1,Max:2) ->> (IZ:5,LSK:-1,B:xx,Min:1,Max:2)",
      testCase "3" $ zeige_spur (spur (GZ tt (stringToRB "[") 0 0)) @?=
        "(IZ:0,LSK:0,B:[,Min:1,Max:1) ->> (IZ:1,LSK:1,B:[,Min:1,Max:1) ->> (IZ:1,LSK:2,B:[,Min:1,Max:1) ->> (IZ:4,LSK:1,B:[,Min:1,Max:1)",
      testCase "4" $ zeige_spur (spur (GZ tt (stringToRB "x") 0 0)) @?=
        "(IZ:0,LSK:0,B:x,Min:1,Max:1) ->> (IZ:1,LSK:1,B:x,Min:1,Max:1) ->> (IZ:1,LSK:2,B:x,Min:1,Max:1) ->> (IZ:4,LSK:1,B:x,Min:1,Max:1) ->> (IZ:4,LSK:0,B:x,Min:1,Max:1) ->> (IZ:5,LSK:-1,B:x,Min:1,Max:1)",
      testCase "5" $ zeige_spur(take 10 (spur (GZ t_endless (stringToRB "x") 0 0))) @?=
        "(IZ:0,LSK:0,B:x,Min:1,Max:1) ->> (IZ:0,LSK:1,B:x,Min:1,Max:1) ->> (IZ:0,LSK:0,B:x,Min:1,Max:1) ->> (IZ:0,LSK:1,B:x,Min:1,Max:1) ->> (IZ:0,LSK:0,B:x,Min:1,Max:1) ->> (IZ:0,LSK:1,B:x,Min:1,Max:1) ->> (IZ:0,LSK:0,B:x,Min:1,Max:1) ->> (IZ:0,LSK:1,B:x,Min:1,Max:1) ->> (IZ:0,LSK:0,B:x,Min:1,Max:1) ->> (IZ:0,LSK:1,B:x,Min:1,Max:1)"
    ]

sim_test :: TestTree
sim_test =
  testGroup
    "sim"
    [
      testCase "1" $ show (sim (SE tt leer_rb)) @?=
        "IZ: 5 // LSKP: -1 // BI: Leer",
      testCase "2" $ show (sim (SE tt (wandle_in_rb "()"))) @?=
        "IZ: 5 // LSKP: -1 // BI: 1>xx<2",
      testCase "3" $ show (sim (SE tt (wandle_in_rb "()[]([([]())])"))) @?=
        "IZ: 5 // LSKP: -1 // BI: 1>xxxxxxxxxxxxxx<14",
      testCase "4" $ show (sim (SE [(0,Blank,Drucke (Z '|'),1),(1,Z '|',Bewege_LSK_nach Links,2)] (wandle_in_rb "||"))) @?=
        "IZ: 2 // LSKP: -1 // BI: 0>|||<2"

    ]

----------------------------------------------------------------
-- helpers
----------------------------------------------------------------

leer_band :: p -> Bandalphabet
leer_band _ = Blank

leer_rb :: Rechenband
leer_rb = RB U leer_band

showRB :: Rechenband -> String
showRB (RB U _) = "unbeschrieben"
showRB (RB (B lo hi) band) = concatMap (showBZ . band) [lo .. hi]

showBZ :: Bandalphabet -> String
showBZ Blank = []
showBZ (Z z) = [z]

bi :: Rechenband -> String
bi (RB U _) = "Leer"
bi (RB (B lo hi) band) = show lo ++ ">" ++ showRB (RB (B lo hi) band) ++ "<" ++ show hi

stringToRB :: [Zeichenvorrat] -> Rechenband
stringToRB cs
  | null cs = RB U leer_band
  | otherwise = RB (B 1 csl) retC
  where
    csl = fromIntegral $ length cs
    retC :: Band
    retC x
      | x > 0 && x <= csl = Z $ cs !! fromIntegral (x - 1)
      | otherwise = Blank

tt :: Turingtafel
tt =
  [ (0, Blank, Bewege_LSK_nach Rechts, 1),
    (1, Z '(', Bewege_LSK_nach Rechts, 1),
    (1, Z '[', Bewege_LSK_nach Rechts, 1),
    (1, Z 'x', Bewege_LSK_nach Rechts, 1),
    (1, Z ')', Drucke (Z 'x'), 2),
    (1, Z ']', Drucke (Z 'x'), 3),
    (1, Blank, Bewege_LSK_nach Links, 4),
    (2, Z 'x', Bewege_LSK_nach Links, 2),
    (2, Z '(', Drucke (Z 'x'), 1),
    (3, Z 'x', Bewege_LSK_nach Links, 3),
    (3, Z '[', Drucke (Z 'x'), 1),
    (4, Z 'x', Bewege_LSK_nach Links, 4),
    (4, Blank, Bewege_LSK_nach Links, 5)
  ]

t_endless :: Turingtafel
t_endless = [
    (0, Blank, Bewege_LSK_nach Rechts, 0),
    (0, Z 'x', Bewege_LSK_nach Links, 0)
  ]
