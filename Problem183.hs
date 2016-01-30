module Problem183 where

import Data.Ratio
import Numbers

term :: Integer -> Bool
term n = r2 == 1
    where n' :: Double
          n' = fromIntegral n
          k0 :: Double
          k0 = fromIntegral $ floor $ n' / exp 1
          k1 = k0 + 1
          k  = floor $ if k0*log (n'/k0) > k1*log (n'/k1) then k0 else k1
          r0 = k `div` (gcd n k)
          (r1,_) = r0 `divMaxPow` 2
          (r2,_) = r1 `divMaxPow` 5

solution :: Integer -> Integer
solution m = sum [ if term n then -n else n | n <- [5..m] ]

main = print $ solution 10000