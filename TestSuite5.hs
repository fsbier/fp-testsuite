module TestSuite5 where

import Test.Tasty
import Test.Tasty.HUnit


-- gueltiger Wahlvorschlag
gwv = [WW "John" "Smith" ABC,
       WW "Judy" "Hall" DEF,
       WW "John" "Doug" MNO]
-- gemischte Wahl
gw = [[1,2,3,4],[2,1,3],[3,4,1],[],[5,1],[1,2,3],[2,3,1],[2,2,1]]
wahl1 = [[2,1,3],[3,4,1],[5,1],[1,2,3],[2,3,1]]
wahl3 = [[2,1,3],[3,4,1],[2,1],[1,2,3],[3,2,1]]

spec :: TestTree
spec =
  testGroup
    "Angabe5 Tests"
    [
      -- A1
      testCase "igW 1" $  ist_gueltiger_Wahlvorschlag [] @?= False,
      testCase "igW 2" $  ist_gueltiger_Wahlvorschlag [WW "John" "Smith" ABC] @?= True,
      testCase "igW 3" $  ist_gueltiger_Wahlvorschlag [WW "John" "Smith" ABC, WW "Judy" "Hall" DEF] @?= True,
      -- A2
      testCase "igS 1" $  ist_gueltiger_Stimmzettel [] [1,2,3] @?= False,
      testCase "igS 2" $  ist_gueltiger_Stimmzettel gwv [] @?= True,
      testCase "igS 3" $  ist_gueltiger_Stimmzettel gwv [1,2,3] @?= True,
      testCase "igS 4" $  ist_gueltiger_Stimmzettel gwv [1,0,3] @?= False,
      testCase "igS 5" $  ist_gueltiger_Stimmzettel gwv [1,4,3] @?= False,
      testCase "igS 6" $  ist_gueltiger_Stimmzettel gwv [1,2,3,4] @?= False,
      testCase "igS 7" $  ist_gueltiger_Stimmzettel gwv [1,2,2] @?= False,
      -- A3
      testCase "tS 1" $  trenne_Stimmzettel [] [[1,2,3]] @?= ([],[]), -- Laut Angabe unklar ob error oder leere Liste?
      testCase "tS 2" $  trenne_Stimmzettel gwv [[2,1,3],[3,4,1],[5,1],[1,2,3],[2,3,1]] @?= ([[2,1,3],[1,2,3],[2,3,1]],[[3,4,1],[5,1]]),
      testCase "tS 3" $  trenne_Stimmzettel gwv [[1,2,3,4],[2,1,3],[3,4,1],[],[5,1],[1,2,3],[2,3,1],[2,2,1]] @?= ([[2,1,3],[],[1,2,3],[2,3,1]],[[1,2,3,4],[3,4,1],[5,1],[2,2,1]]),
      -- A4
      testCase "a 1" $  auszaehlen [] [[1,2,3]] @?= Nothing,
      testCase "a 2" $  auszaehlen gwv [[1,2,3,4],[2,1,3],[3,4,1],[],[5,1],[1,2,3],[2,3,1],[2,2,1]] @?= Nothing,
      testCase "a 3" $  auszaehlen gwv [[],[],[],[]] @?= Just [0,0,0],
      testCase "a 4" $  auszaehlen gwv [[1,2,3],[2,3,1],[3,2,1],[],[2,3,1],[3],[1],[3,2],[1,2]] @?= Just [3,2,3],
      testCase "a 5" $  auszaehlen gwv [[1,2,3],[2,3,1],[2,1],[],[2,3,1],[2],[1],[1,2],[1,2]] @?= Just [4,4,0],
      -- A5
      testCase "ws 1" $  wahlsieger [] (Just [1,2,3]) @?= Nothing,
      testCase "ws 2" $  wahlsieger gwv Nothing @?= Nothing,
      testCase "ws 3" $  wahlsieger gwv (Just [1,2,3,0]) @?= Nothing,
      testCase "ws 4" $  wahlsieger gwv (Just [1,2,3]) @?= Nothing,
      testCase "ws 5" $  wahlsieger gwv (Just [1,2,4]) @?= Just (gwv!!2,3),
      testCase "ws 5" $  wahlsieger gwv (Just [25,23,50]) @?= Just (gwv!!2,3),
      testCase "ws 5" $  wahlsieger gwv (Just [25,23,48]) @?= Nothing,
      -- A6
      testCase "as 1" $  ausscheiden [] [1,2,3] @?= [],
      testCase "as 2" $  ausscheiden gw [] @?= gw, -- Unklar ob "Platz 1"-Stimmen ungueltig gegeben werden kann
      testCase "as 3" $  ausscheiden gw [2,3,1] @?= [[1,2,4],[2,1],[4,1],[],[5,1],[1,2],[2,1],[2,2,1]],
      testCase "as 4" $  ausscheiden [[1,2,4],[2,1],[4,1],[],[3,1],[1,2],[2,1],[2,2,1]] [2,3,2] @?= [[2,4],[2],[4],[],[],[2],[2],[2,2]], -- Aus der Angabe lese ich heraus, dass "KANDIDATEN" (Plural) mit den wenigsten Stimmen entfernt werden sollen, also in dem Fall Kandidat 1 und 3
      testCase "as 5" $  ausscheiden [[2],[1]] [1,1] @?= [[],[]],
      -- A7
      testCase "wa 1" $  wahlausgang gwv wahl1 @?= Gewaehlt_ist (gwv!!1),
      testCase "wa 2" $  wahlausgang gwv wahl3 @?= Gewaehlt_ist (gwv!!1),
      testCase "wa 3" $  wahlausgang gwv [[1,2],[2,1],[2],[1]] @?= Kein_Wahlsieger_Wahlwiederholung,
      testCase "wa 4" $  wahlausgang gwv [[],[],[],[]] @?= Kein_Wahlsieger_Wahlwiederholung,
      testCase "wa 5" $  wahlausgang gwv [[1,1,3],[2,2],[6,1,3],[]] @?= Kein_Wahlsieger_Wahlwiederholung,
      testCase "wa 6" $  wahlausgang gwv [[1,1,3],[2,2],[6,1,3]] @?= Keine_gueltigen_Stimmen,
      testCase "wa 7" $  wahlausgang [] [[1,2],[2,1],[2],[1]] @?= Ungueltiger_Wahlvorschlag,
      testCase "wa 8" $  wahlausgang gwv [[1,2,4],[2,1],[4,1],[],[5,1],[1,2],[2,1],[1,2]] @?= Kein_Wahlsieger_Wahlwiederholung,
      testCase "wa 9" $  wahlausgang gwv [[2,1,3],[3,4,1],[2,1],[3,1,2],[3,2,1]] @?= Kein_Wahlsieger_Wahlwiederholung
    ]
