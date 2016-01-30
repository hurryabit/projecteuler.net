module Main where

import Data.Array
import Data.List

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n =
  all (\p -> n `mod` p /= 0) . takeWhile (\p -> p*p <= n) $ primes

phiArray :: Array Int Int
phiArray = listArray size $ 1:map (phiBy primes) (tail (range size))
  where size = (1,1000000)
        phiBy (p:ps) n
          | n < p*p   = n-1
          | otherwise = case n `divMaxPow` p of
              (q,0) -> phiBy ps n
              (q,k) -> (p-1) * p^(k-1) * phi q

phi :: Int -> Int
phi n = phiArray ! n

count :: Int -> Integer
count = sum . map (fromIntegral . phi) . enumFromTo 2

main = print $ count 1000000

divMaxPow :: (Integral a, Integral e) => a -> a -> (a,e)
divMaxPow = divMaxPow' 0
  where divMaxPow' :: (Integral a, Integral e) => e -> a -> a -> (a,e)
        divMaxPow' k n d = case divMod n d of
          (q,0) -> divMaxPow' (k+1) q d
          (q,_) -> (n,k)
