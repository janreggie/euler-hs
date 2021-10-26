module Solutions.P23 (p23) where

import qualified Data.IntMap.Lazy as IM
import Helpers (sumOfDivisors)

p23 :: String -> Integer
p23 _ = toInteger $ sum $ filter (not . isSumOfTwoAbundant) [1 .. 28123]

data Abundance = Deficient | Perfect | Abundant deriving (Show, Eq)

abundanceWithMemo :: Int -> IM.IntMap Abundance -> (Abundance, IM.IntMap Abundance)
abundanceWithMemo x m =
  case IM.lookup x m of
    Just v -> (v, m)
    Nothing -> (res, IM.insert x res m)
  where
    res = abundance x

abundance :: Int -> Abundance
abundance x
  | x < 2 = Deficient
  | otherwise =
    case compare (sumOfDivisors (toInteger x) - toInteger x) (toInteger x) of
      LT -> Deficient
      EQ -> Perfect
      GT -> Abundant

isSumOfTwoAbundant :: Int -> Bool
isSumOfTwoAbundant x = any bothAbundant [(i, x - i) | i <- [x `div` 2, x `div` 2 - 1 .. 1]]
  where
    bothAbundant :: (Int, Int) -> Bool
    bothAbundant (x, y) = abundance x == Abundant && abundance y == Abundant
