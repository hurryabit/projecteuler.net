module Problem407 where

import Data.Array
import Data.List
import Numbers

factorizations :: Array Int [(Int,Int)]
factorizations = factorizationArray 10000000

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = concat [ [ys,x:ys] | ys <- subsets xs ]

euclid :: Integral a => a -> a -> (a,a,a)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

chinese :: Integral a => (a,a) -> (a,a) -> (a,a)
chinese (a,m) (b,n) = let (1,r,s) = euclid m n
                      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)

idempotents :: Int -> [Int]
idempotents n = do
    fs <- subsets (factorizations ! n)
    let p = foldl' (\a (b,e) -> a*b^e) 1 fs
        q = n `div` p
    return . fst $ chinese (1,p) (0,q)

main = print . sum . map (maximum . idempotents) $ [1..10000000]