module Problem129 where

import Data.List
import Data.Maybe
import Numbers hiding (primes)

ruoPrime :: Int -> Int
ruoPrime p = 1 + fromJust (elemIndex 0 . take p $ iterate (\i -> (10*i+1) `mod` p) 1)

ruo :: Int -> Int
ruo = foldl (\r (p,e) -> lcm r (ruoPrime p*p^(e-1))) 1 . factorization

solution = head [ n | n <- [1000000..], gcd 10 n == 1, ruo n > 1000000 ]

{-
powMod :: Integer -> Integer -> Integer -> Integer
powMod x 0 n = 1
powMod x e n = if r == 0 then (y*y) `mod` n else (x*y*y) `mod` n
  where (q,r) = e `divMod` 2
        y = powMod x q n

main = mapM_ print [ p | p <- primes, powMod 10 (p-1) (p*p) == 1 ]
-}