module Solutions.P4 (p4) where

import Helpers (isqrt)

p4 :: Maybe String -> Integer
p4 _ = head $ filter isProductOfThrees $ map makePalindrome [999, 998 .. 100]

makePalindrome :: Integer -> Integer
makePalindrome x = x * 1000 + (100 * units x) + (10 * tens x) + hundreds x
  where
    units x = x `mod` 10
    tens x = (x `div` 10) `mod` 10
    hundreds x = (x `div` 100) `mod` 10

isProductOfThrees :: Integer -> Bool
isProductOfThrees x = not $ null [x `div` divisor | divisor <- [isq, isq -1 .. 100], x `mod` divisor == 0, x `div` divisor <= 999]
  where
    isq = isqrt x
