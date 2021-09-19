module Solutions.P10 (p10) where

import Helpers (primes)

p10 :: String -> Integer
p10 _ = sum $ takeWhile (< 2000000) primes