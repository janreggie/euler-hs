module Solutions.P3 (p3) where

p3 :: String -> Integer
p3 _ = getLargestPrime 600851475143

getLargestPrime num = iter num 2
  where
    -- iter tries to make n = n/k if n % k == 0
    iter n k
      | n == k = n
      | n `mod` k == 0 = iter (n `div` k) k
      | otherwise = iter n (k + 1)
