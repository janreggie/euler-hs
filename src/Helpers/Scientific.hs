module Helpers.Scientific where

-- | Scientific = Scientific b e == b*10^e
data Scientific = Scientific Double Integer deriving (Eq)

simplify :: Scientific -> Scientific
simplify term@(Scientific b e)
  | ab == 0 = Scientific 0 1
  | ab < 1 = simplify (Scientific (b * 10) (e -1))
  | ab < 10 = term
  | otherwise = simplify (Scientific (b / 10) (e + 1))
  where
    ab = abs b

instance Show Scientific where
  show (Scientific b e) = show b ++ "e" ++ show e

instance Ord Scientific where
  compare (Scientific b1 e1) (Scientific b2 e2)
    | e1 /= e2 = compare e1 e2
    | otherwise = compare b1 b2

instance Num Scientific where
  (+) (Scientific b1 e1) (Scientific b2 e2)
    | e1 < e2 = (+) (Scientific b2 e2) (Scientific b1 e1)
    | e1 == e2 = simplify (Scientific (b1 + b2) e1)
    | otherwise = (+) (Scientific b1 e1) (Scientific (b2 / 10) (e2 + 1))

  (*) (Scientific b1 e1) (Scientific b2 e2) = simplify (Scientific (b1 * b2) (e1 + e2))

  abs (Scientific b e) = Scientific (abs b) e

  signum (Scientific b e) = case compare b 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

  fromInteger x = simplify (Scientific (fromInteger x) 0)

  negate (Scientific b e) = Scientific (negate b) e
