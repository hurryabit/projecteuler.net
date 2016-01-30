module Problem225 where

import Data.Function

tribonacciMod n = iterate (\(x,y,z) -> (y,z,(x+y+z) `mod` n)) (1,1,1)

tribonacciCycle n = (1,1,1):takeWhile (/=(1,1,1)) (tail (tribonacciMod n))

nonDivisor = all (\(t,_,_) -> t /= 0) . tribonacciCycle

nonDivisors = filter nonDivisor [3,5..]
