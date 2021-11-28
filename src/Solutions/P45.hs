module Solutions.P45 (p45) where

p45 :: String -> Integer
p45 _ = iter (286, 166, 144)

iter :: (Integer, Integer, Integer) -> Integer
iter (a, b, c)
  | ta == pb && pb == hc = ta
  | ta <= pb && ta <= hc = iter (a + 1, b, c)
  | pb <= ta && pb <= hc = iter (a, b + 1, c)
  | otherwise = iter (a, b, c + 1)
  where
    (ta, pb, hc) = (triangle a, pentagonal b, hexagonal c)

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)
