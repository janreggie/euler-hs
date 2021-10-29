module Solutions.P2 (p2) where

import Helpers (fibonacci)

-- | Problem 2: Even Fibonacci Numbers
--
-- By considering the terms in the Fibonacci sequence
-- whose values do not exceed four million,
-- find the sum of the even-valued terms.
p2 :: String -> Integer
p2 _ = sum $ takeWhile (< 4000000) $ filter even fibonacci
