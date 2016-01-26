module Problem132 where

import qualified Numbers as N

main = print $ take 40 primeFactors

primes :: [Integer]
primes = drop 3 N.primes

primeFactors :: [Integer]
primeFactors = concatMap (\p -> map (const p) (dividingPowers p)) primes

dividingPowers :: Integer -> [Integer]
dividingPowers p = takeWhile (\k -> powModPrimePower 10 billion p k == 1) [1..]

billion :: Integer
billion = 1000000000

-- powMod x n m = x^n mod m
powMod :: Integer -> Integer -> Integer -> Integer
powMod x 0 m = 1
powMod x n m = if e == 0 then y2 else (x*y2) `mod` m
  where (k,e) = n `divMod` 2
        y = powMod x k m
        y2 = (y*y) `mod` m

-- powModPrime x n p = x^n mod p where p is a prime
powModPrime :: Integer -> Integer -> Integer -> Integer
powModPrime x n p = powMod x (n `mod` (p-1)) p

-- powModPrimePower x n p k = x^n mod p^k where p is a prime
powModPrimePower :: Integer -> Integer -> Integer -> Integer -> Integer
powModPrimePower x n p k = powMod x (n `mod` (pk1*(p-1))) (pk1*p)
  where pk1 = p^(k-1)
