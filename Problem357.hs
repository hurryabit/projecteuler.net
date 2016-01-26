module Problem357 where

import Control.Monad
import Data.Array
import qualified Numbers

limit :: Int
limit = 100000000

primesArray :: Array Int Bool
primesArray = Numbers.isPrimeArray (limit `div` 2)

isPrime :: Int -> Bool
isPrime n
    | 2*n <= limit = primesArray ! n
    | otherwise    = Numbers.isPrimeBy primes n

primes :: [Int]
primes = Numbers.primesFrom primesArray

factorization :: Int -> [(Int,Int)]
factorization = Numbers.factorizationBy primes

space :: [[Int]]
space = []:do
    p <- drop 2 primes
    let pf = factorization (p-2)
    guard $ fst (head pf) /= 2 && all ((1==) . snd) pf
    return $ map fst pf

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = concat [ [x:ys,ys] | ys <- subsets xs ]

solutions :: [Int]
solutions = 1:do
    fs <- space
    let n = 2*product fs
    guard $ all (\gs -> let d = product gs in isPrime (d + n `div` d)) (subsets fs)
    return n

main = print $ sum solutions