module Problem214 where

import Data.Array
import Sieve

totientChainSC :: SieveCombinator Int (Int,Int)
totientChainSC = SieveCombinator
    { one     = (1,1)
    , combine = \p k q (phi_q,_) rec -> let phi_n = (p-1)*p^(k-1)*phi_q in (phi_n,1+snd (rec phi_n))
    , isPrime = \n (phi_n,_) -> phi_n == n-1
    }

solution :: Int
solution = let (table,primes) = sieveTablePrimes totientChainSC 40000000
           in  sum [ p | p <- primes, snd (table ! p) == 24 ]

main :: IO ()
main = print solution