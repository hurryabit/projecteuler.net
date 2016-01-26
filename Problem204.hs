module Main where

import Numbers

sol = f 1000000000 (takeWhile (<100) primes)

f :: Int -> [Int] -> Int
f n (p:ps) | n > 1 = let ds = takeWhile (>0) . iterate (`div` p) $ n
                     in  sum [ f d ps | d <- ds ]
f _ _ = 1

main = print sol
