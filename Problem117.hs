module Problem117 where

import Data.List
import Data.Function

sol = fix (\xs -> 1:1:2:4:zipWith4 (\a b c d -> a+b+c+d) xs (drop 1 xs) (drop 2 xs) (drop 3 xs)) !! 50
