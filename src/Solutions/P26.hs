module Solutions.P26 (p26) where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as S
import Helpers (primes)

-- Because the only numbers that would yield such a relationship
-- would be primes that pass the "cyclic test".
-- See <https://en.wikipedia.org/wiki/Cyclic_number>.

p26 :: String -> Integer
p26 _ = head $ filter isCyclic $ reverse $ takeWhile (< 1000) primes

-- | isCyclic returns true if the number is a cyclic number.
-- It checks if 10^x mod p is unique for all 0<=x<=p-1.
-- Why that? See <https://en.wikipedia.org/wiki/Primitive_root_modulo_n#Elementary_example>.
isCyclic :: Integer -> Bool
isCyclic p = iter 1 (10 `mod` p) S.empty
  where
    iter :: Integer -> Integer -> S.Set Integer -> Bool
    iter ind cur m
      | ind == p = True
      | S.member cur m = False
      | otherwise = iter (ind + 1) ((cur * 10) `mod` p) (S.insert cur m)
