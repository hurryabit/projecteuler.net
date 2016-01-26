module Problem140 where

import Data.Function

isSquare :: Integer -> Bool
isSquare n = n == r*r
    where r = floor . sqrt $ fromIntegral n

goldenNuggets_spec :: [Integer]
goldenNuggets_spec = [ n | n <- [1..], isSquare $ 5*n^2+14*n+1 ]

goldenNuggets :: [Integer]
goldenNuggets = gen 2 21 `shuffle` gen 5 42
    where shuffle (x:xs) (y:ys) = x:y:shuffle xs ys
          gen a1 a2 = fix $ \as -> a1:a2:zipWith (\b1 b2 -> 7*b2-b1+7) as (tail as)

solution = sum $ take 30 goldenNuggets

odds,evens :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x:_:xs) = x:odds xs
evens [] = []
evens [_] = []
evens (_:x:xs) = x:evens xs

quotients :: [Integer] -> [Double]
quotients xs = zipWith (\x y -> fromIntegral x / fromIntegral y) (tail xs) xs

limit = 6.854126174877269