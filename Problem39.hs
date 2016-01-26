module Problem30 where

import Control.Arrow

count :: Int -> Int
count p = length $ filter (\b -> (p*(p-2*b)) `mod` (2*(p-b)) == 0) [1..b']
  where b' = floor (fromIntegral p/(2+sqrt 2))

sol = maximum [ (count p,p) | p <- [1..1000] ]
