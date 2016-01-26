module Problem443 where

import Data.Numbers.Primes

step :: (Int,Int) -> (Int,Int)
step (n,gn) =   let p = head $ primeFactors (gn-n-1)
                    i = p - n `mod` p
                    gn' = gn+i-1
                in  (n+i,gn' + gcd (n+i) gn')

solution m = let (n,gn) = last $ takeWhile ((m >=) . fst) (iterate step (4,13))
             in  gn+m-n

main = print $ solution $ 10^15