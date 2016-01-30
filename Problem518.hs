{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.Array
import Data.Binary
import System.IO.Unsafe

import Numbers

triples :: [(Int,Int,Int)]
triples = do
  a <- takeWhile (< (10^8)) primes
  let a1   = a+1
      m    = maxSquareDivisor a1
      a1m  = a1 `div` m
      a1mm = a1m `div` m
      t    = floor $ fromIntegral (10^4*m) / sqrt (fromIntegral a1)
  s <- [m+1 .. t]
  let b = s*a1m - 1
      c = s*s*a1mm - 1
  guard $ primeTable ! b && primeTable ! c
  return (a,b,c)
      
primeTable :: Array Int Bool
primeTable = isPrimeArray (10^8)

factorizationTable :: Array Int [(Int,Int)]
factorizationTable = factorizationArray (10^8)

maxSquareDivisor :: Int -> Int
maxSquareDivisor = foldr (\(p,k) m -> m*p^k) 1 . map (fmap (`div` 2)) . (factorizationTable !)

main = print $ sum [ a+b+c | (a,b,c) <- triples ]
