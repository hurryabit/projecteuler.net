module Problem50 where

import Data.List

import Primes

seqs :: [(Int,Int)]
seqs = concatMap seqsFor (tails primes)

seqsFor :: [Int] -> [(Int,Int)]
seqsFor = filter (isPrime . snd) . takeWhile ((<1000000) . snd) . tail .
  scanl (\(k,p) q -> (k+1,p+q)) (0,0)
