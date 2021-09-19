module Solutions.P5 (p5) where

import Data.List (foldl')

p5 :: String -> Integer
p5 _ = foldl' lcm 1 [1 .. 20]
