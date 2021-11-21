module Solutions.P41 (p41) where

-- TODO: Currently takes 40 seconds. Can it be made faster?

import Data.List (permutations, sort)
import Helpers (isPrime, primes)

-- Why up to seven digits?
-- Because, by the eighth digit, the sum of the digits will be 36,
-- and the number will automatically be divisible by 9.

allSevenDigits :: [Integer]
allSevenDigits = map mergeDigits $ filter (odd . last) (permutations [1, 2, 3, 4, 5, 6, 7])
  where
    mergeDigits = iter . reverse
    iter [] = 0
    iter [x] = x
    iter (x : xs) = iter xs * 10 + x

p41 :: String -> Integer
p41 _ = maximum $ filter isPrime allSevenDigits
