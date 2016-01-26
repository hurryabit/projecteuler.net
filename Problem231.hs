module Problem231 where

import Data.Int
import Data.List
import qualified Numbers as N

primes :: [Int]
primes = N.primes

sum0 = sum . takeWhile (/=0)

rangeSum :: Int -> Int -> Int64
rangeSum k n = 
