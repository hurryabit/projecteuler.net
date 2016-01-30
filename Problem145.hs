module Problem145 where

f n = 
    sum [20^(k+1)*25^k*5 | k <- [0 .. (n-3) `div` 4] ]
  + sum [ 30^(k-1)*20 | k <- [1..n `div` 2] ]
