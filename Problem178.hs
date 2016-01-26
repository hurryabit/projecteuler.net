module Problem178 where

import qualified Data.MemoCombinators as Memo

count :: Int -> Int -> (Bool,Bool) -> Int
count = Memo.memo3 Memo.integral Memo.integral (Memo.pair Memo.bool Memo.bool) count'
    where count' 0 d (l,u) = 0
          count' r 0 (l,u) = fromEnum u + count (r-1) 1 (True,u)
          count' r 9 (l,u) = fromEnum l + count (r-1) 8 (l,True)
          count' r d (l,u) = fromEnum (l && u) + count (r-1) (d-1) (l,u) + count (r-1) (d+1) (l,u)

main = print $ sum [ count 40 d (False,False) | d <- [1..9] ]
