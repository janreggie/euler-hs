module Solutions.P16 where

import Data.Char (digitToInt)

-- The trivial solution would be
-- toInteger . sum . map digitToInt $ show (2 ^ 1000)
-- Well, it works, but suppose for a second that Integers don't exist.
-- In that case, we'll have to use lists to represent this yuge number.

p16 :: String -> Integer
p16 _ = toInteger $ sum $ pow2 1000

-- pow2 represents 2^e as a list of digits
-- where (head ds) represents the units digit.
pow2 :: Int -> [Int]
pow2 e = iter e [1]
  where
    iter :: Int -> [Int] -> [Int]
    iter 0 ds = ds
    iter e ds = iter (e -1) (double ds False)

-- double "doubles" the number represented as the list of digits
-- with a given carry flag.
-- The carry can only be at most 1.
double :: [Int] -> Bool -> [Int]
double [] True = [1]
double [] False = []
double (d : ds) carry = u : double ds (t == 1)
  where
    (t, u) = divMod (d * 2 + c) 10
    c = if carry then 1 else 0
