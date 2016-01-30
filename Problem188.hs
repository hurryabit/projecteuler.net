module Problem188 where

import Data.List

powers :: [(Integer,Integer)]
powers = tail . zip [0..] . iterate (\n -> (1777*n) `mod` 100000000) $ 1

sol1 = find ((1==) . snd) powers -- == 1250000

towers :: [Integer]
towers = iterate f 1777
  where f n = let k = n `mod` 1250000
              in  powMod 1777 k 100000000

powMod :: Integer -> Integer -> Integer -> Integer
powMod n 0 m = 1
powMod n k m
  | even k = ((powMod n (k `div` 2) m)^2) `mod` m
  | odd k  = (powMod n (k-1) m * n) `mod` m
