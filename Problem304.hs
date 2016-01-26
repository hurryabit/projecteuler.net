module Problem304 where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.Numbers.Primes

start, end :: Int
start = 100000000000000
end = 4000000

modulus :: Integer
modulus = 1234567891011

largePrimes :: [Int]
largePrimes = take 100000 $ [ n | (n,b) <- assocs large, b ]
    where
    large = runSTUArray $ do
                arr <- newArray (0,end) True
                forM_ (takeWhile (10000000 >) primes) $ \p -> do
                    forM_ (takeWhile (end >=) $ iterate (p +) ((-start) `mod` p)) $ \n ->
                        writeArray arr n False
                return arr

type SMat22 = (Integer,Integer,Integer)

(#*) :: SMat22 -> SMat22 -> SMat22
(a11,a12,a22) #* (b11,b12,b22) = (a11*b11+a12*b12,a11*b12+a12*b22,a12*b12+a22*b22)

(#%) :: SMat22 -> Integer -> SMat22
(a11,a12,a22) #% m = (a11 `mod` m,a12 `mod` m,a22 `mod` m)

(#^) :: SMat22 -> Int -> SMat22
(#^) = run (1,0,1)
    where
    run y x 0 = y
    run y x k = let (q,r) = k `divMod` 2
                in  run (if r == 0 then y else y #* x) (x #* x) q

multMod :: Integer -> SMat22 -> SMat22 -> SMat22
multMod m (a11,a12,a22) (b11,b12,b22) = ((a11*b11+a12*b12) `mod` m,(a11*b12+a12*b22) `mod` m,(a12*b12+a22*b22) `mod` m)

powMod :: Integer -> SMat22 -> Int -> SMat22
powMod m = run (1,0,1)
    where
    run y x 0 = y
    run y x k = let (q,r) = k `divMod` 2
                in  run (if r == 0 then y else multMod m y x) (multMod m x x) q


solution = foldl' (\s f -> let (_,a12,_) = multMod modulus f1014 f in (s+a12) `mod` modulus) 0 fs
    where f1014 = powMod modulus (0,1,1) (10^14)
          fs    = map (powMod modulus (0,1,1)) largePrimes

main = print solution

fibs = map (\(_,a12,_) -> a12) $ iterate ((0,1,1) #*) (1,0,1)