module Problem206 where

import Data.List

square n = n*n

digits 1 = [3,7]
digits k =
  filter (isOk . square) [ p2*a+b | a <- [0..99], b <- digits' ]
  where digits' = digits (k-2)
        p2 = 10^(k-2)
        p1 = 10*p2
        p0 = 10*p1
        d  = 9 - k `div` 2
        isOk n = (n `mod` p0) `div` p1 == d

sol = fmap (10*) . find (isSolution . square) . dropWhile (<10^8) . digits $ 9

isSolution n = "123456789" == unfoldr f (show n)
  where f xs = if null xs then Nothing else Just (head xs,drop 2 xs)
