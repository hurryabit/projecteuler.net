module Problem158 where

f :: Integer -> Integer
f n = (product [26-n+1..26] `div` product [1..n]) * (2^n-n-1)

solution = maximum $ map f [1..26]
