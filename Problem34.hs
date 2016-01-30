module Main where

import Data.Char

isSolution :: Int -> Bool
isSolution n = n == sum (map (fac 1 . digitToInt) (show n))

fac p 0 = p
fac p n = fac (p*n) (n-1)

main = print $ filter isSolution [10..2177282]
