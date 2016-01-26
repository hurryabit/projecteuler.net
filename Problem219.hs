module Problem219 where

import Data.List
import qualified Data.MemoCombinators as Memo

main = print $ g 1000000000

f :: Integer -> Integer
f = Memo.integral f'
  where f' 1 = 1
        f' 2 = 5
        f' n = min simple split
          where simple = f (n-1) + n-1 + 4
                split = firstMinimum [ f (n-k) + n + 3*k + f k | k <- [1..] ]

firstMinimum :: Ord a => [a] -> a
firstMinimum (x:xs) = run x xs
  where run y [] = y
        run y (z:zs)
          | y > z     = run z zs
          | otherwise = y

g 1 = 1
g n = run (n-2) (zip pattern' [6..]) 5
  where run k ((l,d):lds) r
          | k <= l    = r + k*d
          | otherwise = run (k-l) lds (r+l*d)

--sizes :: [Integer]
--sizes = map f [2..]

--sizes' = scanl (+) 5 (concat $ zipWith replicate pattern' [6..])

--diffs xs = zipWith (-) (tail xs) xs

--pattern = map length . group $ diffs sizes

pattern' = 1:zipWith (+) pattern' (0:0:1:pattern')
