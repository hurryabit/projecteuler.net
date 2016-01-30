module Problem46 where

import Primes

isGoldbach :: Int -> Bool
isGoldbach n = any isPrime $ takeWhile (>0) [ n-2*k*k | k <- [1..] ]

counterExamples :: [Int]
counterExamples =
  filter (\n -> not (isPrime n || isGoldbach n)) [9,11..1000000]
