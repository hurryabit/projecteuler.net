module Problem52 where

import Data.List

digits :: Integer -> String
digits = sort . show

sols n = filter isSolution [10^(n-1) .. 10^n `div` 6]

isSolution :: Integer -> Bool
isSolution k =
  let s = digits k
  in  all (s==) (map (digits . (k*)) [2..6])
