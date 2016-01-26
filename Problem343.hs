module Problem343 where

import Data.MemoCombinators
import Data.Numbers.Primes

size = 10000

main = print $ sum [ max (maxPrimeFactor (k+1)) (maxPrimeFactor (k*k-k+1)) - 1 | k <- [1 .. size] ]

maxPrimeFactor :: Int -> Int
maxPrimeFactor = switch (<= size') (arrayRange (2,size')) bits mpf'
    where size' = 10000000

mpf' n = case [ q | p <- takeWhile (\p -> p*p <= n) primes, let (q,r) = n `divMod` p, r == 0 ] of
    []    -> n
    (q:_) -> maxPrimeFactor q