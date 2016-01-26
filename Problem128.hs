module Problem128 where

import Data.Numbers.Primes (primes, isPrime)

main = print $ (1:2:concat [ (if all isPrime [6*r-1,6*r+1,12*r+5] then [2+3*(r-1)*r] else []) ++ (if all isPrime [6*r-1,6*r+5,12*r-7] then [1+3*r*(r+1)] else []) | r <- [2..] ]) !! 1999
