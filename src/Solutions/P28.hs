module Solutions.P28 (p28) where

-- 500 because the 500th ring is 1001x1001

p28 :: String -> Integer
p28 _ = 1 + sum (map sumRing [1 .. 500])

-- ring returns the four numbers per "ring" x units outside the centre.
-- e.g., ring 1 == (3,5,7,9)
-- ring 2 = (13,17,21,25)
ring :: Integer -> (Integer, Integer, Integer, Integer)
ring x = (v + d, v + 2 * d, v + 3 * d, v + 4 * d)
  where
    v = 4 * x * x - 4 * x + 1
    d = 2 * x

-- sumRing returns the sum of the four numbers per ring x units out of centre
sumRing :: Integer -> Integer
sumRing = s . ring
  where
    s (a1, a2, a3, a4) = a1 + a2 + a3 + a4
