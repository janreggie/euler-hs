module Solutions.P48 (p48) where

p48 :: String -> Integer
p48 _ = m10 $ sum (map (\x -> m10 (x ^ x)) [1 .. 1000])

m10 x = x `mod` (10 ^ 10)
