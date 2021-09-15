module Main where

import Solver (solver)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

data Answer = Answer
  { problemNumber :: Integer,
    inputText :: Maybe String,
    expected :: Integer
  }

answers :: [Answer]
answers =
  [ Answer 1 Nothing 233168
  ]

testCases = map toTestCase answers
  where
    toTestCase ans@(Answer probNo inputTxt expected) =
      testCase
        ("Testing P" ++ show probNo)
        (assertEqual (display ans) expected (solver probNo inputTxt))

    display (Answer probNo inputTxt expected) =
      "Problem " ++ show probNo ++ showInput inputTxt ++ " should result to " ++ show expected
    showInput Nothing = ""
    showInput (Just s) = " with input " ++ s

main :: IO ()
main = do
  defaultMain (testGroup "Solutions tests" testCases)
