module Solutions.P30 (p30) where

-- Why 354294?
-- fifthPowerDigitSum 99999 = 295245
-- fifthPowerDigitSum 999999 = 354294
-- Any number x > 999999 will have a fifthPowerDigitSum less "impressive" than x.

p30 :: String -> Integer
p30 _ = sum $ filter (\x -> x == fifthPowerDigitSum x) [2 .. 354294]

fifthPowerDigitSum :: Integer -> Integer
fifthPowerDigitSum 0 = 0
fifthPowerDigitSum x = first ^ 5 + fifthPowerDigitSum rest
  where
    (rest, first) = x `divMod` 10
