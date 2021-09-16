module Solutions.P6 (p6) where

p6 :: Maybe String -> Integer
p6 _ = sq (sum [1 .. x]) - sum (map sq [1 .. x])
  where
    x = 100
    sq x = x * x

-- Boring solution
--
-- p6 _ = sq (x * (x + 1)) `div` 4 - (x * (x + 1) * (2 * x + 1)) `div` 6
--   where
--     x = 100
--     sq x = x * x