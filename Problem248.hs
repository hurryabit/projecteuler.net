module Problem248 where

import Data.List
import Data.Numbers.Primes (isPrime)

type Number = [(Int,Int)]

factorizations :: Number -> [(Number,Number)]
factorizations []     = [ ([],[]) ]
factorizations ((p,k):pks) = [ ((p,l):pls,(p,k-l):pms) | l <- [0..k], (pls,pms) <- factorizations pks ]

primeFactorizations :: Int -> Number -> [(Int,Number)]
primeFactorizations q []          = [ (0,[]) ]
primeFactorizations q ((p,k):pks)
    | p <  q = [ (m,(p,k):pls) | (m,pls) <- primeFactorizations q pks ]
    | p == q = [ (m,(p,k-m):pks) | m <- [0..k] ]
    | p >  q = [ (0,(p,k):pks) ]

unPhi :: Number -> [Int]
unPhi = run 1
    where run n pks
            | all ((0 ==) . snd) pks  = if n == 1 then [1,2] else [1]
            | otherwise               = [ r * q^(k+1) 
                                        | (pls,pms) <- factorizations pks
                                        , let q = foldl' (\r (p,k) -> r*p^k) 1 pls + 1
                                        , q > n && isPrime q
                                        , (k,pns) <- primeFactorizations q pms
                                        , r <- run q pns
                                        ]

main = print $ sort (unPhi [(2,10),(3,5),(5,2),(7,1),(11,1),(13,1)]) !! 149999
