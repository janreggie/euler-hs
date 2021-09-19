module Solutions.P9 (p9) where

p9 :: String -> Integer
p9 _ = a * b * c
  where
    (a, b, c) = getTriple

getTriple :: (Integer, Integer, Integer)
getTriple =
  head
    [ (a, b, c)
      | c <- [1000, 999 .. 1],
        b <- [1 .. 1000 - c -1],
        a <- [1000 - c - b],
        a * a + b * b == c * c
    ]
