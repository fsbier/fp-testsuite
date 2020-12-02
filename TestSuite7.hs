module TestSuite7 where

import Angabe7 hiding (leer_band, leer_rb, showBZ, showRB)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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

spec :: TestTree
spec =
  testGroup
    "Angabe7 Tests"
    [ akt_band_tests,
      akt_rechenband_test,
      wandle_in_rb_test,
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

sim_test :: TestTree
sim_test =
  testGroup
    "sim"
    [ testCase "1" $ show (sim (SE tt (wandle_in_rb "()"))) @?= "IZ: 5 // LSKP: -1 // BI: 1>xx<2",
      testCase "2" $ show (sim (SE tt (wandle_in_rb "()[]([([]())])"))) @?= "IZ: 5 // LSKP: -1 // BI: 1>xxxxxxxxxxxxxx<14"
    ]
