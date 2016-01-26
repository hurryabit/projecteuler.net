module Problem82 where

import Data.List
import System.IO.Unsafe

matrix :: [[Int]]
matrix = transpose . map f . lines . unsafePerformIO $ readFile "matrix.txt"
  where f xs = read $ "[" ++ xs ++ "]"

costs :: [[Int]] -> [[Int]]
costs = scanl1 f
  where f us cs = fix g (zipWith (+) us cs)
          where g vs = zipWith4 h us (maxBound:vs) (tail vs ++ [maxBound]) cs
                h u r l c = min u (min r l) + c

fix :: Eq a => (a -> a) -> a -> a
fix f x = fst . head . filter (uncurry (==)) $ zip ys (tail ys)
  where ys = iterate f x
