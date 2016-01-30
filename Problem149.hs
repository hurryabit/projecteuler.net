module Problem149 where

import Data.Array

main = print . maximum . map maxSubSum $ ways

maxSubSum :: (Num a,Ord a) => [a] -> a
maxSubSum = snd . foldl f (0,0) . scanl (+) 0
  where f (s,d) x
          | x < s     = (x,d)
          | d' > d    = (s,d')
          | otherwise = (s,d)
          where d' = x-s

s :: [Integer]
s = [ (100003-200003*k+300007*k*k*k) `mod` 1000000 - 500000 | k <- [1..55] ] ++
    zipWith (\a b -> (a+b) `mod` 1000000 - 500000) (drop 31 s) s


table :: Array (Int,Int) Integer
table = listArray ((0,0),(1999,1999)) s

ways :: [[Integer]]
ways =  [ [table ! (i,j) | j <- [0..1999]] | i <- [0..1999] ] ++
        [ [table ! (i,j) | i <- [0..1999]] | j <- [0..1999] ] ++
        [ [table ! (i,i-d) | i <- [max 0 d..1999+min 0 d]] | d <- [-1999..1999] ] ++
        [ [table ! (i,d-i) | i <- [max 0 (d-1999)..min d 1999]] | d <- [0..3998] ]
