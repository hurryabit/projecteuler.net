module Problem190 where

import Data.Ratio

sol = sum [ p m | m <- [2..15] ]

p :: Int -> Int
p m = let t :: Rational
          t = 2 / fromIntegral (m + 1)
      in  floor $ product [ (fromIntegral k * t)^k | k <- [1..m] ]
