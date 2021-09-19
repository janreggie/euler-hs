module Solutions.P1 (p1) where

-- | Problem 1: Multiples of 3 or 5
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- This is a simple exclusion-inclusion principle problem.
p1 :: String -> Integer
p1 _ = sumOfMultiples 3 999 + sumOfMultiples 5 999 - sumOfMultiples 15 999

-- | find the sum of multiples of k that are less than n.
--
-- sumOfMultiples 3 10 == 3 + 6 + 9 == 15
--
-- Make sure that k and n are positive. Otherwise, returns zero.
sumOfMultiples :: Integer -> Integer -> Integer
sumOfMultiples k n
  | k > 0 && n > 0 = k * count * (count + 1) `div` 2
  | otherwise = 0
  where
    count = n `div` k
