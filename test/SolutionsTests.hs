module Main where

import Solver (solver)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

data Answer = Answer
  { problemNumber :: Integer,
    inputText :: String,
    expected :: Integer
  }

answers :: [Answer]
answers =
  [ Answer 1 "" 233168,
    Answer 2 "" 4613732,
    Answer 3 "" 6857,
    Answer 4 "" 906609,
    Answer 5 "" 232792560,
    Answer 6 "" 25164150,
    Answer 7 "" 104743,
    Answer 8 "" 23514624000,
    Answer 9 "" 31875000,
    Answer 10 "" 142913828922,
    Answer 11 "" 70600674,
    Answer 12 "" 76576500,
    Answer 13 "" 5537376230,
    Answer 14 "" 837799,
    Answer 15 "" 137846528820,
    Answer 16 "" 1366
  ]

testCases = map toTestCase answers
  where
    toTestCase ans@(Answer probNo inputTxt expected) =
      testCase
        ("Testing P" ++ show probNo)
        (assertEqual (display ans) expected (solver probNo inputTxt))

    display (Answer probNo inputTxt expected) =
      "Problem " ++ show probNo ++ showInput inputTxt ++ " should result to " ++ show expected
    showInput "" = ""
    showInput s = " with input " ++ s

main :: IO ()
main = do
  defaultMain (testGroup "Solutions tests" testCases)
