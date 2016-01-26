module Problem303 where

import ListPlus

next :: Integer -> Integer
next m
    | m `mod` 10 < 2 = m+1
    | otherwise      = 10 * next (m `div` 10)

f :: Integer -> Integer
f n = maybe (error "should not happen") id . find (\m -> m `mod` n == 0) $ iterate next 1

solution = sum (map (\n -> f n `div` n) [1..9998]) + 11112222222222222222 `div` 9999 + 1

main = print solution