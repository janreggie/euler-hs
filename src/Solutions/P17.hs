module Solutions.P17 (p17) where

import Data.Char (isAsciiLower)

p17 :: String -> Integer
p17 _ = totalLetterCount

totalLetterCount :: Integer
totalLetterCount = sum (map hundreds [0 .. 9]) + oneThousand

-- XXX hundred + XXX hundred and (hundreds 0)
hundreds :: Int -> Integer
hundreds 0 = sum $ map tens [0 .. 9]
hundreds x = leading + 99 * (leading + countLetters "and") + hundreds 0
  where
    leading = countLetters (["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (x -1)) + countLetters "hundred"

tens :: Int -> Integer
tens 0 = sum $ map countLetters ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tens 1 = sum $ map countLetters ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens x = 10 * countLetters (["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] !! (x -2)) + tens 0

oneThousand :: Integer
oneThousand = countLetters "one thousand"

countLetters :: String -> Integer
countLetters = toInteger . length . filter isAsciiLower
