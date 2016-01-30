module Problem60 where

import Control.Monad
import Data.Array
import qualified Numbers

limit :: Int
limit = 30000

isPrimeArray :: Array Int Bool
isPrimeArray = Numbers.isPrimeArray limit

primes :: [Int]
primes = Numbers.primesFrom isPrimeArray

isPrime :: Int -> Bool
isPrime n
    | n <= limit = isPrimeArray ! n
    | otherwise  = Numbers.isPrimeBy primes n


goodPair :: Int -> Int -> Bool
goodPair m n = let dm = Numbers.digits m
                   dn = Numbers.digits n
                   dmn = Numbers.foldDigits (dm++dn)
                   dnm = Numbers.foldDigits (dn++dm)
               in  isPrime dmn && isPrime dnm

ppsets2 :: Array Int [[Int]]
ppsets2 = array (1,limit) $ do
    n <- [1..limit]
    let n' = n `div` 2
        sets = [ [p1,p2] | p1 <- takeWhile (< n') (tail primes), let p2 = n - p1, isPrime p2, goodPair p1 p2]
    return (n,sets)

ppsets3 :: Array Int [[Int]]
ppsets3 = array (1,limit) $ do
    n <- [1..limit]
    let n' = n `div` 3
        sets = [ p:ps | p <- takeWhile (< n') (tail primes), ps <- ppsets2 ! (n-p), p < head ps, all (goodPair p) ps ]
    return (n,sets)

ppsets4 :: Array Int [[Int]]
ppsets4 = array (1,limit) $ do
    n <- [1..limit]
    let n' = n `div` 4
        sets = [ p:ps | p <- takeWhile (< n') (tail primes), ps <- ppsets3 ! (n-p), p < head ps, all (goodPair p) ps ]
    return (n,sets)

ppsets5 :: Array Int [[Int]]
ppsets5 = array (1,limit) $ do
    n <- [1..limit]
    let n' = n `div` 5
        sets = [ p:ps | p <- takeWhile (< n') (tail primes), ps <- ppsets4 ! (n-p), p < head ps, all (goodPair p) ps ]
    return (n,sets)

main = print . fst . head . filter (not . null . snd) . assocs $ ppsets5
