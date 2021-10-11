module Solutions.P20 (p20) where

import Data.Char (digitToInt)

p20 :: String -> Integer
p20 _ = toInteger $ sum $ map digitToInt $ show (fac 100)
  where
    fac 1 = 1
    fac x = x * fac (x -1)
