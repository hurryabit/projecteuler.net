module Problem174 where

import Data.Array.IArray
import Numbers

lArray :: Array Int Int
lArray = amap (`div` 2) $ numDivisorsArray 250000
