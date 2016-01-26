module Main where

import Factorizations

isAbundant :: Int -> Bool
isAbundant n = d n > n

abundants :: [Int]
abundants = filter isAbundant [1..30000]

isSolution :: Int -> Bool
isSolution n = all (not . isAbundant) . takeWhile (>0) . map (n-) $ abundants

main = do
  let sols = filter isSolution [1..28123]
  print sols
  print (sum sols)
