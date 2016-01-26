module Problem323 where

import Data.Ratio

binomial :: Integer -> Integer -> Integer
binomial n k = foldl (\b (r,s) -> (b*r) `div` s) 1 . zip [n,n-1..] $ [1..k']
    where k' = if k+k < n then k else n-k

probN :: Integer -> Rational
probN 0 = 0
probN i = sum [ fromIntegral (binomial 32 k) * (1-(1%2)^(i-1))^k * ((1%2)^(i-1))^(32-k) * (1%2)^(32-k) | k <- [0..31] ]

expN :: [Rational]
expN = tail $ scanl (\e i -> e + probN i * fromIntegral i) 0 [0..]

solution = let es :: [Double]
               es = map fromRational expN
           in  fst . head . filter (uncurry (==)) $ zip es (tail es)