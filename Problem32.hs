module Problem32 where

import Data.List

triples :: [(Int,Int,Int)]
triples =
  [ (a,b,c) | a <- [10..99], b <- [100..999], let c=a*b, 1000<=c && c<10000 ]
  ++
  [ (a,b,c) | a <- [2..9], b <- [1000..4999], let c=a*b, 1000<=c && c<10000 ]

isPandigital :: (Int,Int,Int) -> Bool
isPandigital (a,b,c) = unify (concatMap show [a,b,c]) == ['1'..'9']

unify :: Ord a => [a] -> [a]
unify = map head . group . sort
