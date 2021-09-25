module Solutions.P15 (p15) where

-- Lattice paths is a combinatorics problem

p15 :: String -> Integer
p15 _ = choose 40 20

-- choose n k
choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k
