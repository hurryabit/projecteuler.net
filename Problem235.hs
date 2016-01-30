module Problem235 where

import Data.Ratio

f :: Double -> Double
f r = let r5000 = r^5000
      --in  ((-14100)*r*r5000+14003*r5000-900*r+897)/(r-1)^2 -- + 600000000000 % 1
      in  900*(r5000-1)/(r-1) - 3*(5000*r5000*(r-1)-(r5000-1))/(r-1)^2 + 6e11

u :: Rational -> Int -> Rational
u r k = (900-3*fromIntegral k)*r^(k-1)

s :: Rational -> Rational
s r = sum $ map (u r) [1..5000]

--improve :: (Integer,Integer) -> (Integer,Integer)
--improve (l,u) = if f (m%(10^13)) < 0 then (l,m) else (m,u)
--  where m = (l+u) `div` 2
