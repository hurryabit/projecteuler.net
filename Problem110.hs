module Main where

import qualified Numbers as N

solutions :: Integer -> [Integer]
solutions m = sols 1 1 maxExponent primes
  where target        = 2*m-1
        primeCount    = ceiling (logBase 3 (fromIntegral target))
        primes        = take primeCount N.primes
        maxExponent   = ceiling (logBase 2 (fromIntegral (product primes)))

        sols :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
        sols n f emax ps
          | f >= target = [n]
        sols n f emax [] = []
        sols n f emax (p:ps) =
          concat [ sols (n*p^e) (f*(2*e+1)) e ps | e <- [1..emax] ]

main = print . minimum . solutions $ 4000000
