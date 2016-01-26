module Problem119 where

import Control.Monad
import Data.List
import Numbers

range :: Integer -> [Integer]
range k = concat . takeWhile (not . null) .
  map (\l -> [10^(l-1)..min (9*k*l) (10^l-1)]) $ [1..]

solutionsFor :: Integer -> [Integer]
solutionsFor k = do
  x <- range k
  let n = x^k
  guard $ digitSum n == x && n >= 10
  return n

solutionsUpTo :: Integer -> [Integer]
solutionsUpTo k = sort . concat . map solutionsFor $ [2..k]

solution = solutionsUpTo 10 !! 29
