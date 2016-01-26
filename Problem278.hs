module Problem278 where

import Data.List
import Numbers

f (p,q,r) = 2*p*q*r - p*q - p*r - q*r

primes5000 :: [Integer]
primes5000 = takeWhile (<5000) primes

pairs :: [a] -> [(a,a)]
pairs xs = concat $ zipWith (map . (,)) xs (tails (tail xs))

triples :: [a] -> [(a,a,a)]
triples xs = concat $ zipWith (\y -> map (uncurry ((,,) y)) . pairs) xs (tails (tail xs))

solution = sum . map f . triples $ primes5000

main = print solution
