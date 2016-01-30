module Problem267 where

import Data.List
import Data.Maybe
import Data.Ratio

maxCapital :: Int -> Rational
maxCapital k' =
  let k = fromIntegral k'
      a = (3*k) % 1000
      b = (3000-3*k) % 2000
  in  a^k * b^(1000-k)

solution :: Double
solution =
  let index = head $ filter ((1000000000 <=) . maxCapital) [334..]
      binomials = take index $
                    scanl (\b k -> b * (1001-k) / k) 1 [(1 :: Rational)..]
  in  fromRational $ 1 - sum binomials / 2^1000
