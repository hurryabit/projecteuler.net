module Problem159 where

import Data.Array
import Numbers (divisorsArray, digits)

digitalRoot :: Int -> Int
digitalRoot n
    | n < 10    = n
    | otherwise = digitalRoot . sum . digits $ n

divisors :: Array Int [Int]
divisors = divisorsArray 999999

mdrs :: Array Int Int
mdrs = array (1,999999) $ (1,0):do
    n <- [2..999999]
    return (n,maximum $ [ digitalRoot d + mdrs ! (n `div` d) | d <- divisors ! n, d /= 1 ])

solution :: Int
solution = sum (elems mdrs)

main = print solution
