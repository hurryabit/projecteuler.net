module Problem203 where

import Data.List
import Numbers

pascal :: [[Integer]]
pascal = [1]:map (\r -> zipWith (+) (0:r) (r++[0])) pascal

primeSquares :: [Integer]
primeSquares = map (\n -> n*n) . takeWhile (<50) $ primes

sol = sum . filter (\n -> all (\q -> q `notDivides` n) primeSquares) .
        map head . group . sort . concat . take 51 $ pascal
