module Problem157 where

import Data.List
import Numbers

solve :: Int -> [(Integer,Integer,Integer)]
solve n = sol1 ++ sol2
    where sol1 = [ (a,b,p)
                 | k <- [0..n]
                 , l <- [0..n]
                 , let pd = (2^k+5^l)*2^(n-k)*5^(n-l)
                 , p <- divisors pd
                 , let d = pd `div` p
                       a = d*2^k
                       b = d*5^l
                 ]
          sol2 = [ (a,b,p)
                 | k <- [1..n]
                 , l <- [1..n]
                 , let pd = (1+2^k*5^l)*2^(n-k)*5^(n-l)
                 , p <- divisors pd
                 , let d = pd `div` p
                       a = d
                       b = d*2^k*5^l
                 ]

solution = sum [ length (solve n) | n <- [1..9] ]