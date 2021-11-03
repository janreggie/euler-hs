module Solutions.P25 (p25) where

import Helpers (fibonacci)
import Helpers.Scientific (Scientific (..))

p25 :: String -> Integer
p25 _ = f1 0 1 1

-- f1 is the first stage of the search,
-- going through the numbers in logarithmic time.
-- Note that ind is the index of cur.
f1 :: Scientific -> Scientific -> Integer -> Integer
f1 prev cur ind
  | prev' > comp = f2 prev cur ind
  | otherwise = f1 prev' cur' (ind * 2)
  where
    prev' = prev * prev + cur * cur
    cur' = 2 * prev * cur + cur * cur

-- f2 scans through things linearly.
f2 :: Scientific -> Scientific -> Integer -> Integer
f2 prev cur ind
  | cur >= comp = ind
  | otherwise = f2 cur (prev + cur) (ind + 1)

comp :: Scientific
comp = Scientific 1 999

-- Alternative solution
-- This is "as fast as" the previous one,
-- but consider that we're zipping through 4782 terms,
-- and this one consumes 23MB of memory while the prev one consumes 1.2MB.
p25o = snd $ head $ dropWhile (\x -> fst x < (10 ^ 999)) $ zip fibonacci [0 ..]
