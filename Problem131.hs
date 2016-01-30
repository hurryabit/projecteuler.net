module Problem131 where

import qualified Numbers as N

primes :: [Int]
primes = N.primes

solution = length . filter (N.isPrimeBy primes) . takeWhile (<1000000) .
  map (\x -> 3*x*x+3*x+1) $ [1..]
