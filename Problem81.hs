module Problem81 where

import Data.Function
import System.IO.Unsafe

matrix :: [[Int]]
matrix = map f . lines . unsafePerformIO $ readFile "matrix.txt"
  where f xs = read $ "[" ++ xs ++ "]"

costs :: [[Int]] -> [[Int]]
costs (xs:xss) = fix $ (scanl1 (+) xs :) . zipWith f xss
  where f cs (u:us) = rs
          where ms = u:zipWith min rs us
                rs = zipWith (+) ms cs
