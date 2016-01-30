module Factorizations where

import Control.Applicative
import Data.Array

import Primes

factorizations :: Array Int [(Int,Int)]
factorizations = listArray size (map (factorBy primes) (range size))
  where size = (1,500000)
        factorBy ::[Int] -> Int -> [(Int,Int)]
        factorBy _      1 = []
        factorBy (p:ps) n
          | n < p*p   = [(n,1)]
          | otherwise = case divMaxPow n p of
                          (q,0) -> factorBy ps n
                          (q,k) -> (p,k):(factorizations ! q)

divMaxPow :: (Integral a, Integral e) => a -> a -> (a,e)
divMaxPow = divMaxPow' 0
  where divMaxPow' :: (Integral a, Integral e) => e -> a -> a -> (a,e)
        divMaxPow' k n d = case divMod n d of
          (q,0) -> divMaxPow' (k+1) q d
          (q,_) -> (n,k)

divisors :: Int -> [Int]
divisors n = foldl f [1] (factorizations ! n)
  where f :: [Int] -> (Int,Int) -> [Int]
        f ds (p,k) = (*) <$> take (k+1) (iterate (*p) 1) <*> ds

d :: Int -> Int
d n = sum (divisors n) - n
