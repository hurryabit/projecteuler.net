module Problem169 where

import Data.List

bits :: Integer -> [Integer]
bits = unfoldr f
  where f 0 = Nothing
        f n = let (q,r) = n `divMod` 2 in Just (r,q)

value :: Integer -> [Integer] -> Integer
value n [1] = n
value 1 (1:xs) = value 1 xs
value n (1:xs) = (n-1) * value 2 xs + value 1 xs
value n (0:xs) = value (n+1) xs
