-- Gould's sequence, A001316 in OEIS
module Problem242 where

import Data.Function
import Data.List
import qualified Data.MemoCombinators as Memo

binom :: Integer -> Integer -> Integer
binom n k
    | k' < 0    = 0
    | otherwise = foldl (\r (a,b) -> (r*a) `div` b) 1 $ zip [n,n-1..] [1..k']
    where k' = if 2*k <= n then k else n-k
--    | k <  0 || k >  n = 0
--    | k == 0 || k == n = 1
--    | otherwise        = Memo.memo2 Memo.integral Memo.integral (\m l -> binom (m-1) (l-1) + binom (m-1) l) n k

f :: Integer -> Integer -> Integer
f n k = sum [ binom ((n+1) `div` 2) i * binom (n `div` 2) (k-i) | i <- [1,3..k] ]

count :: Integer -> Integer
count n = genericLength [ k | k <- [1,3..n], odd (f n k) ]

gouldSeq :: [Integer]
gouldSeq = concat . fix $ ([1]:) . map (map (2*)) . scanl1 (++)

gouldSum_spec :: Integer -> Integer
gouldSum_spec = sum . flip genericTake gouldSeq

gouldSum :: Integer -> Integer
gouldSum 0 = 0
gouldSum n = p3 + 2*gouldSum (n-p2)
    where (p2,p3) = last . takeWhile ((n >=) . fst) $ iterate (\(q2,q3) -> (2*q2,3*q3)) (1,1)