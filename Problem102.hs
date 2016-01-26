module Problem102 where

import Data.List
import Data.List.Utils
import System.IO.Unsafe

triangles :: [[(Double,Double)]]
triangles = map (unfoldr f . map read . split ",") . lines . unsafePerformIO $
  readFile "triangles.txt"
  where f (x1:x2:xs) = Just ((x1,x2),xs)
        f _          = Nothing

above0 :: (Double,Double) -> (Double,Double) -> Bool
above0 (x1,y1) (x2,y2) = sin (atan2 y1 x1 - atan2 y2 x2) > 0

interior0' :: [(Double,Double)] -> Bool
interior0' [a,b,c] = b `above0` a && c `above0` b && a `above0` c

flipX :: (Double,Double) -> (Double,Double)
flipX (x,y) = (x,negate y)

interior0 :: [(Double,Double)] -> Bool
interior0 ps = any interior0' [ps,map flipX ps]
