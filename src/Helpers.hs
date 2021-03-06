module Helpers where

import qualified Data.IntMap.Lazy as IM

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

-- | returns whether a number is prime
isPrime :: Integer -> Bool
isPrime x
  | x < 2 = False
  | otherwise = iter 2 x
  where
    iter d x
      | d >= mx = True
      | x `rem` d == 0 = False
      | otherwise = iter (d + 1) x
    mx = isqrt x

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

-- | sum of all divisors of a number, including itself
sumOfDivisors :: Integer -> Integer
sumOfDivisors x = product (zipWith tt primes (primeFactorization x))
  where
    tt :: Integer -> Integer -> Integer
    tt p e = sum $ map (p ^) [0 .. e]

-- | fibonacci numbers. Starts with [0,1,1,...]
fibonacci :: (Num a) => [a]
fibonacci = fibo 0 1
  where
    fibo a b = a : fibo b (a + b)

-- | mergeDigits [1,2,3] = 123
mergeDigits :: Num a => [a] -> a
mergeDigits = iter . reverse
  where
    iter [] = 0
    iter [x] = x
    iter (x : xs) = iter xs * 10 + x
