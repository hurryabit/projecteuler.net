module Problem25 where

import Data.List

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

refInteger :: Integer
refInteger = 10^999

digits1000 :: Integer -> Bool
digits1000 = (>= refInteger)

sol = find (digits1000 . snd) (zip [0..] fibs)
