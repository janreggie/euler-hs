module Solutions.P38 (p38) where

import Data.Char (chr, ord)
import Data.List (sort)

p38 :: String -> Integer
p38 _ = case twopans of
  [] -> fivepan
  (x : __) -> max fivepan x

-- Suppose we have three sorts of numbers:
-- twopan : (number) x (1,2) == (show number ++ show (2*number))
-- threepan : (number) x (1,2,3) == (show number ++ show (2*number) ++ show (3*number))
-- fourpan : (number) x (1,2,3,4)
-- fivepan also exists, but as the (sole) 918273645.
--
-- Honestly, we don't care about threepans ([100..333]) and fourpans ([25..33])
-- since whatever value they'll make will be less than twopans or the sole fivepan.

twopans :: [Integer]
twopans = map read (filter isPandigital [show x ++ show (2 * x) | x <- [9999, 9998 .. 5000]])

fivepan :: Integer
fivepan = 918273645

isPandigital :: String -> Bool
isPandigital = iter '1' . sort
  where
    iter _ [] = True
    iter c (x : xs)
      | x == '0' = False
      | x /= c = False
      | otherwise = iter (next c) xs
    next c = chr (ord c + 1)
