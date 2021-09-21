module Solutions.P12 where

import Helpers (countDivisors)

p12 :: String -> Integer
p12 _ = head $ dropWhile (\x -> countDivisors x <= 500) [x * (x + 1) `div` 2 | x <- [1 ..]]
