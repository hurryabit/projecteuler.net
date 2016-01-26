module Problem61 where

import Control.Monad
import Data.List hiding (nub)

nGonals :: Int -> [Int]
nGonals n = scanl1 (+) [1,n-1..]

nGonalsX :: Int -> [Int]
nGonalsX = filter (\n -> n `mod` 100 >= 10) .
  takeWhile (<10000) . dropWhile (<1000) . nGonals

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs):[ (y,x:ys) | (y,ys) <- select xs ]

solve :: [[Int]]
solve = do
  n8 <- nGonalsX 8
  map (n8:) (subSolve n8 [3..7] n8)

subSolve :: Int -> [Int] -> Int -> [[Int]]
subSolve z [] x = guard (z `div` 100 == x `mod` 100) >> return []
subSolve z ts x = do
  (t,ts') <- select ts
  nt <- nGonalsX t
  guard $ nt `div` 100 == x `mod` 100
  map (nt:) (subSolve z ts' nt)
