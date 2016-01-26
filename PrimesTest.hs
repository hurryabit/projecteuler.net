module PrimesTest where

import Data.Array.Unboxed

primesFrom :: UArray Int Bool -> [Int]
primesFrom pa = [ p | (p,True) <- assocs pa ]

isPrimeBy :: [Int] -> Int -> Bool
isPrimeBy ps n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p<=n) ps

primeArray1 :: Int -> UArray Int Bool
primeArray1 n = let pa  = array (1,n) $ (1,False):(2,True):[(k,isPrimeBy ps k) | k <- [3..n]]
                    ps  = primesFrom pa
                in  pa

limit :: Int
limit = 10000000

primes1 :: [Int]
primes1 = 2:filter (isPrimeBy primes1) [3..limit]

primes2 :: [Int]
primes2 = sieve [2..limit]
    where sieve (p:ns) = p:sieve (filter (\n -> n `mod` p /= 0) ns)

main = print $ sum primes2