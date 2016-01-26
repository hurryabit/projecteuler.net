module Problem77 where

import qualified Numbers as N

primes :: [Int]
primes = N.primes

count :: [Int] -> Int -> Int
count (p:ps) n
  | p > n     = 0
  | p == n    = 1
  | otherwise = count (p:ps) (n-p) + count ps n
