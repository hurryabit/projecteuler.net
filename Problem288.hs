module Problem288 where

import Data.List

main = print $ nfMod 61 10000000 10

powers :: Integer -> [Integer]
powers p = iterate (*p) 1

series :: Integer -> [Integer]
series = scanl1 (+) . powers

seqS :: [Integer]
seqS = iterate (\s -> (s*s) `mod` 50515093) 290797

seqT :: Integer -> [Integer]
seqT p = map (`mod` p) seqS

addMod :: Integer -> Integer -> Integer -> Integer
addMod m x y = (x + y) `mod` m

sumMod :: Integer -> [Integer] -> Integer
sumMod m = foldl1' (addMod m)

nfMod :: Integer -> Int -> Int -> Integer
nfMod p n m = let (_:rs1,rs2) = splitAt m (seqT p)
                  (qs,q:_) = splitAt (m-1) (series p)
                  p2m = p^m
                  sum1 = sumMod p2m $ zipWith (*) rs1 qs
                  sum2 = sumMod p2m $ take (n-m+1) rs2
              in  (sum1 + sum2 * q) `mod` p2m
