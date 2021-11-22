module Solutions.P41 (p41) where

import Data.List (permutations, sort)
import Helpers (isPrime)

-- Why up to seven digits?
-- Because, by the eighth digit, the sum of the digits will be 36,
-- and the number will automatically be divisible by 9.

allSevenDigits :: [Integer]
allSevenDigits = (reverse . sort) $ map mergeDigits $ filter (odd . last) (permutations [1 .. 7])
  where
    mergeDigits = iter . reverse
    iter [] = 0
    iter [x] = x
    iter (x : xs) = iter xs * 10 + x

p41 :: String -> Integer
p41 _ = head $ filter isPrime allSevenDigits
