module Problem111 where

import qualified Numbers as N

primes :: [Integer]
primes = N.primes

isPrime :: Integer -> Bool
isPrime = N.isPrimeBy primes

candidates 0 = [1000000000*a+b | a<-[1..9],b<-[1..9]]
candidates d0
  | d0 /= 2 && d0 /= 8
      = [1111111111*d0+10^k1*(d1-d0) | d1 <- [0..9], d1 /= d0, k1 <- [0..9], d1 /= 0 || k1 /= 9]
  | otherwise
      = [1111111111*d0+10^k1*(d1-d0)+10^k2*(d2-d0)
        | d1 <- [0..9], d1 /= d0, d2 <- [0..9], d2 /= d0
        , k1 <- [0..9], k2 <- [0..k1-1], d1 /= 0 || k1 /= 9]

s_10 = sum . filter isPrime . candidates

solution = sum $ map s_10 [0..9]
