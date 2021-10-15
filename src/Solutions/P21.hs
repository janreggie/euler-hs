module Solutions.P21 (p21) where

import qualified Data.IntMap.Lazy as IM
import Data.Maybe (fromMaybe)
import qualified Helpers as H

p21 :: String -> Integer
p21 _ = sum $ IM.filterWithKey isAmicable allSumsOfDivisors

allSumsOfDivisors :: IM.IntMap Integer
allSumsOfDivisors = IM.fromList [(fromInteger x, H.sumOfDivisors x - x) | x <- [1 .. 9999]]

isAmicable :: Int -> Integer -> Bool
isAmicable x y =
  y < 10000
    && y > 0
    && toInteger x /= y
    && allSumsOfDivisors IM.! fromInteger y == toInteger x
