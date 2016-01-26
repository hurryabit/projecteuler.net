module Problem381 where

import Data.Numbers.Primes

euclid :: Integer -> Integer -> (Integer,Integer,Integer)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

invMod :: Integer -> Integer -> Integer
invMod a n = let (1,r,s) = euclid a n in r

funS :: Integer -> Integer
funS p =  sum (scanl (\r q -> r * invMod q p) (-1) [p-1,p-2..p-4]) `mod` p

main = print . sum . map funS . takeWhile (<10^8) . drop 2 $ primes