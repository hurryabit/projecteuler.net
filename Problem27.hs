module Problem27 where

import Problem10

candidates = [ ((a,b),n)  | b   <- tail (takeWhile (<1000) primes)
                          , a'  <- tail (takeWhile (<(1001+b)) primes)
                          , let a = a'-1-b
                          , let n = consecutivePrimes a b
                          ]

consecutivePrimes :: Int -> Int -> Int
consecutivePrimes a b = length . takeWhile (\p -> p > 0 && isPrime p) $
  [ n*n+a*n+b | n <- [0..] ]
