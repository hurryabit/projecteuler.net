module Problem233 where

import Control.Monad
import Data.List
import Data.Ord

import Data.Numbers.Primes

isSquare :: Integer -> Bool
isSquare k = let r = floor (sqrt (fromIntegral k)) in r*r == k

f_spec :: Integer -> Integer
f_spec n = genericLength [ x | x <- [l..n-1], isSquare (n*n-x*x) ]
    where l = ceiling $ fromIntegral n / sqrt 2

primes1, primes3 :: [Integer]
primes1 = filter (\p -> p `mod` 4 == 1) primes
primes3 = filter (\p -> p `mod` 4 == 3) primes

main = print $ solutions $ 10^11

solutions :: Integer -> Integer
solutions lim = sum [ b*sum (extensions (2:primes3) (lim `div` b)) | b <- bases lim ]

bases :: Integer -> [Integer]
bases lim = concat [ primBases fs (sum fs) primes1 lim | fs <- fss ]
    where fss = concatMap permutations [[1,2,3],[2,10],[3,7]]

primBases :: [Int] -> Int -> [Integer] -> Integer -> [Integer]
primBases []     sfs ps     lim = [1]
primBases (f:fs) sfs (p:ps) lim
    | p^sfs <= lim              = let p2f = p^f
                                  in  map (p2f*) (primBases fs (sfs-f) ps (lim `div` p2f)) ++ primBases (f:fs) sfs ps lim
    | otherwise                 = []

extensions :: [Integer] -> Integer -> [Integer]
extensions (q:qs) lim
    | q <= lim  = [ q2g*qs2gs | q2g <- takeWhile (<= lim) (iterate (q*) 1), qs2gs <- extensions qs (lim `div` q2g) ]
    | otherwise = [1]