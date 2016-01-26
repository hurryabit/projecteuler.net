module Problem87 where

import qualified Data.IntSet as Set

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) . takeWhile (\p -> p*p <= n) $ primes

limit = 50000000

sols = Set.size . Set.fromList $ nums [4,3,2] 0

nums [] n     = [n]
nums (e:es) n = concatMap (nums es) . takeWhile (<= limit) .
                  map (\p -> p^e+n) $ primes
