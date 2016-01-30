module Problem301 where

solution = fibs !! 31
  where fibs = 1:1:zipWith (+) fibs (tail fibs)
