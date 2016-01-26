module Problem63 where

powers k = takeWhile (\(p,n) -> 10^(n-1) <= p) [ (k^n,n) | n <- [1..] ]

sol = length $ concatMap powers [1..9]
