module Problem21 where

import Problem10
import Problem12

amicable :: Int -> Bool
amicable n = let m = d n in n /= m && n == d m

sol = sum . filter amicable $ [2..10000]
