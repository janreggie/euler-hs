module Helpers (isqrt, primes) where

(^!) :: Num a => a -> Int -> a
(^!) x n = x ^ n

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

-- | infinite list of primes (2, 3, 5, 7...)
primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve [] _ = []
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs
