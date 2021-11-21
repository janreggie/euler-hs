module Solutions.P14 (p14) where

import qualified Data.IntMap as IM

p14 :: String -> Integer
p14 _ = findFurthest (1, 0) IM.empty [1 .. 999999]

findFurthest :: (Int, Integer) -> IM.IntMap Integer -> [Int] -> Integer
findFurthest (holder, value) memo [] = toInteger holder
findFurthest (holder, value) memo (x : xs)
  | value' > value = findFurthest (x, value') memo' xs
  | otherwise = findFurthest (holder, value) memo' xs
  where
    (value', memo') = collatzWithMemo x memo

-- | gets the Collatz number of a given number,
-- e.g., map collatz [1, 2, 4, 8, 16, 5] = [0, 1, 2, 3, 4, 5]
collatz :: Int -> Integer
collatz x
  | x == 1 = 0
  | even x = 1 + collatz (x `div` 2)
  | otherwise = 1 + collatz (3 * x + 1)

-- | collatz but with memoization in the form of a Map.
-- See Solutions.P14.p14 on how this is used.
collatzWithMemo :: Int -> IM.IntMap Integer -> (Integer, IM.IntMap Integer)
collatzWithMemo x memo = case IM.lookup x memo of
  Just v -> (toInteger v, memo)
  Nothing
    | x == 1 -> (0, IM.insert 1 0 memo)
    | otherwise -> (1 + v', IM.insert x v' m)
    where
      x' = if even x then x `div` 2 else 3 * x + 1
      (v', m) = collatzWithMemo x' memo
