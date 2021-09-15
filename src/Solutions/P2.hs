module Solutions.P2 (p2) where

-- | Problem 2: Even Fibonacci Numbers
--
-- By considering the terms in the Fibonacci sequence
-- whose values do not exceed four million,
-- find the sum of the even-valued terms.
p2 :: Maybe String -> Integer
p2 _ = sum $ takeWhile (< 4000000) $ filter even fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
