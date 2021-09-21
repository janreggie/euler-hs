module Helpers where

-- | integer square root
isqrt :: Integral p => p -> p
isqrt 0 = 0
isqrt 1 = 1
isqrt n =
  let twopows = iterate (^! 2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (isqrt (div n lowerN) * lowerRoot)
      isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
   in head $ dropWhile (not . isRoot) iters
  where
    (^!) :: Num a => a -> Int -> a
    (^!) x n = x ^ n

-- | infinite list of primes (2, 3, 5, 7...)
primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve [] _ = []
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

-- | prime factorization of a number such that
--
--  product $ zipWith (^) (primeFactorization x) primes == x.
primeFactorization :: Integer -> [Integer]
primeFactorization x = iter x 0 primes
  where
    iter :: Integer -> Integer -> [Integer] -> [Integer]
    iter 1 cur _
      | cur == 0 = []
      | otherwise = [cur]
    iter x cur (p : ps)
      | x `mod` p == 0 = iter (x `div` p) (cur + 1) (p : ps)
      | otherwise = cur : iter x 0 ps
    iter x cur [] = [] -- this will never happen since primes is infinite

-- | counts the number of divisors of a number
countDivisors :: Integer -> Integer
countDivisors = product . map (+ 1) . primeFactorization
