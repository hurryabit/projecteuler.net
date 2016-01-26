module Main where --Problem48 where

{-powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod a 0 m = 1
powMod a k m
  | even k    = powMod ((a*a) `mod` m) (k `div` 2) m
  | otherwise = (a * powMod a (k-1) m) `mod` m-}

main = print $ sum (map (\k -> k^k) [1..1000]) `mod` (10^10)
