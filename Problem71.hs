module Main where

import Data.Ratio

fractions :: [Rational]
fractions =
  [ r | d <- [1..1000000], let n = (3*d) `div` 7, let r = n % d, r < 3%7 ]

main = print $ maximum fractions
