module Main where 

import Data.Array.Unboxed
import Data.List

digits :: Integral a => a -> [a]
digits = unfoldr f
  where f 0 = Nothing
        f n = let (q,r) = n `divMod` 10 in Just (r,q)
  
finishes :: UArray Int Bool
finishes = listArray (0,567) . (False:) . map f $ [1..567]
  where f = (Just 89 ==) . find (\k -> k == 1 || k == 89) .
              iterate (squareSum . digits)

squareSum = sum . map (\n -> n*n)

count n = length . filter (finishes !) . map sum . sequence .
  replicate n . map (^2) $ [0..9]

main = print . count $ 7
