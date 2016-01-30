module Problem12 where

import Data.Array

import Problem10

--primes2 :: [Integer]
--primes2 = sieve $ 2:[3,5..]
--  where sieve (p:ns) = p:sieve (filter (\n -> n `mod` p /= 0) ns)

divMaxPow :: (Integral a, Integral e) => a -> a -> (a,e)
divMaxPow = divMaxPow' 0
  where divMaxPow' :: (Integral a, Integral e) => e -> a -> a -> (a,e)
        divMaxPow' k n d = case divMod n d of
          (q,0) -> divMaxPow' (k+1) q d
          (q,_) -> (n,k)

numDivisorArray :: Array Int Int
numDivisorArray = array size [ (n,numDivisors' primes n) | n <- range size ]
  where size = (1,100000)
        numDivisors' :: [Int] -> Int -> Int
        numDivisors' _      1 = 1
        numDivisors' (p:ps) n
          | n < p*p = 2
          | otherwise = case divMaxPow n p of
                          (q,0) -> numDivisors' ps n
                          (q,k) -> (k+1) * numDivisorArray ! q

numDivisors :: Int -> Int
numDivisors = (numDivisorArray !)
--numDivisors = product . map ((1+) . snd) . factor

numDivisorsTriangle :: Int -> Int
numDivisorsTriangle n
  | even n    = numDivisors (n `div` 2) * numDivisors (n+1)
  | otherwise = numDivisors n * numDivisors ((n+1) `div` 2)

sols = filter ((500 <) . numDivisorsTriangle) [1..99999]

divisorNums = map numDivisors [1..]

divisorNums2 = zipWith (*) divisorNums (tail divisorNums)
