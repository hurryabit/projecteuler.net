module Problem218 where

import Control.Monad

minmax :: Ord a => a -> a -> (a,a)
minmax x y = if x <= y then (x,y) else (y,x)

solutions :: [(Integer,Integer)]
solutions = do
    y <- takeWhile (\z -> 2*z*z < 10^8) [3,5 ..]
    x <- takeWhile (\z -> z*z+y*y < 10^8) [1,3 .. y-2]
    guard $ gcd x y == 1
    let (k,uk) = minmax ((x*x-y*y) `div` 2) (x*y)
        u = uk - k
        v = uk + k
        a = (v*v-u*u) `div` 2
        b = u*v
        f = (a*b) `div` 2
    guard $ f `mod` 84 /= 0
    return (a,b)

main = print $ length solutions