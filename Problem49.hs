module Problem49 where

import Primes
import Data.List

sub :: Int -> [a] -> [[a]]
sub 0 _ = [[]]
sub n [] = []
sub n (x:xs) = map (x:) (sub (n-1) xs) ++ sub n xs

perms :: Int -> [Int]
perms = map head . group . sort . map read . permutations . show

primePerms :: Int -> [Int]
primePerms = filter isPrime . perms

isArithmetic :: [Int] -> Bool
isArithmetic xs = let ds = zipWith (-) xs (tail xs)
                  in  and $ zipWith (==) ds (tail ds)

arithmeticTriples :: Int -> [[Int]]
arithmeticTriples = filter isArithmetic . sub 3 . primePerms

sols = concatMap arithmeticTriples $ dropWhile (<1000) primes
