{-# LANGUAGE BangPatterns #-}
module Problem249 where

import Data.List
import Data.Numbers.Primes

{-# INLINE precision #-}
precision :: Int
precision = 10000000000000000

{-# INLINE add #-}
add :: Int -> Int -> Int
add !x !y = if z' < 0 then z else z'
    where !z = x+y
          !z' = z-precision

solve :: [Int] -> Int
solve ps = foldl' add 0 (select (count ps) (takeWhile (<= sum ps) primes))

zipAdd :: [Int] -> [Int] -> [Int]
zipAdd (x:xs) (y:ys) = let z = add x y in z `seq` z:zipAdd xs ys
zipAdd []     ys     = ys

count :: [Int] -> [Int]
count ps = foldl' (\cs p -> zipAdd cs (replicate p 0 ++ cs)) [1,0,1] (tail ps)

select :: [Int] -> [Int] -> [Int]
select xs is = snd (mapAccumL (\(d,ys) i -> let z:zs = drop (i-d) ys in ((i+1,zs),z)) (0,xs) is)

main = print $ solve (takeWhile (< 3000) primes)