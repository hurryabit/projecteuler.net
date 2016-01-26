module Problem28 where

diffList = concatMap (replicate 4) [2,4..]

valList = 1:zipWith (+) valList diffList

sol n = sum . take (2*n-1) $ valList
