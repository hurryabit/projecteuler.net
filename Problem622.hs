module Problem622 where

import Data.List
import Data.Maybe

solve :: Int -> Int
solve n = 1 + fromJust (elemIndex 2 $ tail $ iterate f 2)
  where
    f k
      | k <= n = 2*k-1
      | otherwise = 2*(k-n)

main = mapM_ print $ scanl (+) 0 [ 2*i | i <- [1..], solve i == 60 ]
