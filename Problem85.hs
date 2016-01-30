module Problem85 where

import Data.List
import Data.Function

f :: Int -> Int -> Int
f m n = (m*(m+1)*n*(n+1)) `div` 4

g :: Int -> [(Int,Int,Int)]
g n = let m = floor $ sqrt (1/4 + 8000000 / fromIntegral (n*(n+1))) - 1/2
      in  [(m,n,f m n),(m+1,n,f (m+1) n)]
gs = concatMap g [1..2000]

sol = minimumBy (compare `on` c) gs
  where c (_,_,s) = abs (2000000-s)
