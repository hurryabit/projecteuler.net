module Main where

import Data.Array
import Data.List
import Data.Ord

primes :: [Int]
primes = 2:filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n =
  all (\p -> n `mod` p /= 0) . takeWhile (\p -> p*p <= n) $ primes

phiArray :: Array Int Double
phiArray = listArray size $ 1:map (phiBy primes) (tail (range size))
  where size = (1,1000000)
        phiBy (p:ps) n
          | n < p*p   = fromIntegral n / fromIntegral (n-1)
          | otherwise = case n `divMaxPow` p of
              (q,0) -> phiBy ps n
              (q,k) -> fromIntegral p / fromIntegral (p-1) * phi q

phi :: Int -> Double
phi n = phiArray ! n

sol n = maximumBy (comparing snd) . take n . assocs $ phiArray

main = print $ sol 1000000

divMaxPow :: (Integral a, Integral e) => a -> a -> (a,e)
divMaxPow = divMaxPow' 0
  where divMaxPow' :: (Integral a, Integral e) => e -> a -> a -> (a,e)
        divMaxPow' k n d = case divMod n d of
          (q,0) -> divMaxPow' (k+1) q d
          (q,_) -> (n,k)
