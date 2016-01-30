module Problem108 where

import Data.List
import qualified Numbers as N

solsBF :: Integer -> Integer
solsBF n = genericLength [ k | k <- [1..n], (n*(n+k)) `mod` k == 0 ]

solsFM :: Integer -> Integer
solsFM = product . map ((1+) . (2*) . snd) . N.factorization

primes :: [Integer]
primes = N.primes

smallestWith :: Integer -> Integer
smallestWith = product . zipWith (^) primes . map (`div` 2) .
  concatMap (uncurry (flip replicate)) . reverse . N.factorizationBy primes

exceeds :: Integer -> [Integer]
exceeds n = map smallestWith [2*n-1,2*n+1..]

sol = print . minimum . take 1000 . exceeds $ 1000
