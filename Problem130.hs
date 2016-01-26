module Problem130 where

import Problem129 hiding (solution)
import Numbers

solution = sum $ take 25 [ n | n <- [7,9..], gcd 5 n == 1, not (isPrime n), (n-1) `mod` (ruo n) == 0 ]