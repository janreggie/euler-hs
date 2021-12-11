module Solutions.P24 (p24) where

import Helpers (mergeDigits)

p24 :: String -> Integer
p24 _ = mergeDigits (permutations [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] !! 999999)

-- permutations returns the list of permutations from some list.
-- e.g., permutations [0,1,2] = [[0,1,2], [0,2,1], ...]
-- It is guaranteed that the output is sorted if the input is.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x : ys | (x, rest) <- takeDropList xs, ys <- permutations rest]

-- takeDropList [0,1,2,3] = [(0,[1,2,3]), (1,[0,2,3]), (2,[0,1,3]), (3,[0,1,2])]
takeDropList :: [a] -> [(a, [a])]
takeDropList xs = iter [] xs
  where
    iter :: [a] -> [a] -> [(a, [a])]
    iter _ [] = []
    iter ys (x : xs) = (x, ys ++ xs) : iter (ys ++ [x]) xs
