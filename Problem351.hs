module Problem351 where

import Data.Array

import Numbers

h :: Int -> Int
h n = 6 * (g n + n - 1)

g :: Int -> Int
g n =
    let phi = phiArray (n `div` 2)
    in  sum [ (n `div` k - 1) * p | (k,p) <- tail (assocs phi) ]

main = print $ h 100000000