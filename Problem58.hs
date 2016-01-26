module Problem58 where

import Control.Arrow
import Data.List

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n =
  all (\p -> n `mod` p /= 0) . takeWhile (\p -> p*p <= n) $ primes

diagonals = scanl (+) 1 . concatMap (replicate 4) $ [2,4..]

primeCount = unfoldr (Just . (head &&& drop 4)) . scanl (+) 0 .
  map (fromEnum . isPrime) . tail $ diagonals

sol = take 10 . dropWhile (\(n,c) -> 10*c >= 2*n-1) . zip [3,5..] . tail $
  primeCount
