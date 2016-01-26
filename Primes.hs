module Primes where

import Data.Array

primeArray :: Array Int Bool
primeArray = listArray size $ False:True:map isPrime' (drop 2 (range size))
  where size = (1,10000)

primes :: [Int]
primes = map fst . filter snd . assocs $ primeArray

isPrime :: Int -> Bool
isPrime p = primeArray ! p

isPrime' :: Int -> Bool
isPrime' p = all (\m -> p `mod` m /= 0) (takeWhile (\m -> m*m <=p) primes)
