module TestSuite7 where

import Angabe7
  (wandle_in_rb, Band, Zeichenvorrat,  Bandalphabet (Blank, Z),
    MinMax (B, U),
    Rechenband (..),
    akt_band,
    akt_rechenband,
  )
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
  | otherwise = RB (B 0 $ csl - 1) retC
  where
    retC :: Band
    retC x
      | x >= 0 && x < csl = Z $ cs !! fromIntegral x
      | otherwise = Blank
    csl = fromIntegral $ length cs

spec :: TestTree
spec =
  testGroup
    "Angabe7 Tests"
    [
      akt_band_tests,
      akt_rechenband_test,
      wandle_in_rb_test
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
    [
      testCase "1" $ bi (akt_rechenband leer_rb 0 Blank) @?= "Leer",
      testCase "2" $ bi (akt_rechenband leer_rb 0 (Z 'X')) @?= "0>X<0",
      testCase "3" $ bi (stringToRB "Test Wort") @?= "0>Test Wort<8",
      testCase "4" $ bi (akt_rechenband (stringToRB "A") 1 (Z 'X')) @?= "0>AX<1",
      testCase "5" $ bi (akt_rechenband (stringToRB "A") (-1) (Z 'X')) @?= "-1>XA<0",
      testCase "6" $ bi (akt_rechenband (stringToRB "A") 0 Blank) @?= "Leer",
      testCase "7" $ bi (akt_rechenband (stringToRB "A") 1 Blank) @?= "0>A<0",
      testCase "8" $ bi (akt_rechenband (stringToRB "ABC") 0 Blank) @?= "1>BC<2",
      testCase "10" $ bi (akt_rechenband (stringToRB "ABC") 2 Blank) @?= "0>AB<1",
      testCase "12" $ bi (akt_rechenband (stringToRB "ABC") (-1) (Z 'X')) @?= "-1>XABC<2",
      testCase "13" $ bi (akt_rechenband (stringToRB "ABC") 0 (Z 'X')) @?= "0>XBC<2",
      testCase "14" $ bi (akt_rechenband (stringToRB "ABC") 1 (Z 'X')) @?= "0>AXC<2",
      testCase "15" $ bi (akt_rechenband (stringToRB "ABC") 2 (Z 'X')) @?= "0>ABX<2",
      testCase "16" $ bi (akt_rechenband (stringToRB "ABC") 3 (Z 'X')) @?= "0>ABCX<3",
      testCase "18" $ bi (akt_rechenband (akt_rechenband (stringToRB "ABC") 2 Blank) 1 Blank) @?= "0>A<0",
      testCase "19" $ bi (akt_rechenband (akt_rechenband (stringToRB "ABC") 0 Blank) 1 Blank) @?= "2>C<2"
    ]

wandle_in_rb_test :: TestTree
wandle_in_rb_test =
  testGroup
    "wandle_in_rb"
    [
      testCase "1" $ bi (wandle_in_rb "") @?= "Leer",
      testCase "2" $ bi (wandle_in_rb "A") @?= "0>A<0",
      testCase "3" $ bi (wandle_in_rb "ABC") @?= "0>ABC<2",
      testCase "4" $ bi (wandle_in_rb "Test Wort") @?= "0>Test Wort<8"
    ]
