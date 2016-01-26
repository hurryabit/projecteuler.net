module Problem134 where

import qualified Numbers as N

primes :: [Integer]
primes = N.primes

euklid :: Integral a => a -> a -> (a,a,a)
euklid a 0 = (a,1,0)
euklid a b = (d,l,k-l*q)
  where (q,r) = a `divMod` b
        (d,k,l) = euklid b r

main = print solution

solution :: Integer
solution = sum . map (uncurry solutionFor) .
  takeWhile ((<=1000000) . fst) . drop 2 $ zip primes (tail primes)

solutionFor :: Integer -> Integer -> Integer
solutionFor p1 p2 = l*k10+p1
  where k = ceiling . logBase 10 . fromIntegral $ p1
        k10 = 10^k
        (_,x,_) = euklid k10 p2
        l = ((-p1)*x) `mod` p2
