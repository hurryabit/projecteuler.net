module Problem9 where

import Data.Maybe (mapMaybe)

triples :: Int -> [(Int,Int,Int)]
triples s = mapMaybe (triple s) [1 .. floor (fromIntegral s/(2+sqrt 2))]

triple :: Int -> Int -> Maybe (Int,Int,Int)
triple s a
  | r1 == 0 && r2 == 0 = Just (a,b,c)
  | otherwise          = Nothing
  where t = s-a
        (q,r1) = (a*a) `divMod` t
        (b,r2) = (t-q) `divMod` 2
        c = t-b
