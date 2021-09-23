module Solutions.P14 (p14) where

import qualified Data.IntMap as IM
import Helpers (collatz, collatzWithMemo)

p14 :: String -> Integer
p14 _ = findFurthest (1, 0) IM.empty [1 .. 999999]

findFurthest :: (Int, Integer) -> IM.IntMap Integer -> [Int] -> Integer
findFurthest (holder, value) memo [] = toInteger holder
findFurthest (holder, value) memo (x : xs)
  | value' > value = findFurthest (x, value') memo' xs
  | otherwise = findFurthest (holder, value) memo' xs
  where
    (value', memo') = collatzWithMemo x memo

-- The following solution may actually be *faster* than using a memo!
-- p14 _ = iter (1, 0) [1 .. 999999]
-- iter :: (Int, Integer) -> [Int] -> Integer
-- iter (holder, value) [] = toInteger holder
-- iter (holder, value) (x : xs)
--   | v' > value = iter (x, v') xs
--   | otherwise = iter (holder, value) xs
--   where
--     v' = collatz x
