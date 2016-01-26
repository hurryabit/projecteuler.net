module Problem43 where

import Data.List

isInteresting :: Integer -> Bool
isInteresting n = all (0==) $ zipWith mod ns [17,13,11,7,5,3,2]
  where ns = map (`mod` 1000) (iterate (`div` 10) n)

sols = filter (\n -> isInteresting n) . map read $
  permutations ['0'..'9']
