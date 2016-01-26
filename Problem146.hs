module Problem146 where

import Data.Numbers.Primes (primes)

allPrime :: [Int] -> Bool
allPrime ns@(n0:_) = all (\p -> all (\n -> n `mod` p /= 0) ns) $ takeWhile (\p -> p*p <= n0) primes

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primes

solution = sum [ n | n <- space, allPrime $ map ((n*n)+) [27,13,9,7,3,1], all (not . isPrime) $ map ((n*n)+) [11,17,19,21,23]]
    where space = takeWhile (<150000000) [ m+n | m <- [0,210..], n <- [10,80,130,200] ]

main = print solution