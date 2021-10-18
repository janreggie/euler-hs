module Solutions.P22 (p22) where

import Data.Char (ord)
import Data.List (sort)
import Data.List.Split (splitOn)

p22 :: String -> Integer
p22 = sum . zipWith (*) [1 ..] . map nameScore . sort . parse

parse :: String -> [String]
parse = map (init . tail) . splitOn ","

nameScore :: String -> Integer
nameScore = toInteger . sum . map (\x -> ord x - ord 'A' + 1)
