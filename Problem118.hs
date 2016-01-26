module Problem118 where

import Control.Monad

import qualified Numbers as N

primes :: [Int]
primes = N.primes

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = concat [ [x:ys,ys] | ys <- subsets xs ]

partitions :: [a] -> [([a],[a])]
partitions [] = [([],[])]
partitions (x:xs) = concat [ [(x:ys,zs),(ys,x:zs)] | (ys,zs) <- partitions xs ]

select :: [a] -> [(a,[a])]
select [x]    = [(x,[])]
select (x:xs) = (x,xs):map (\(y,ys) -> (y,x:ys)) (select xs)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
    (y,ys) <- select xs
    zs <- permutations ys
    return $ y:zs

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = case x `compare` y of
                        LT -> x:merge xs     (y:ys)
                        EQ -> x:merge xs     ys
                        GT -> y:merge (x:xs) ys

count :: [Int] -> [Int] -> [[Int]]
count []    ds
    | null ds          = [[]]
    | otherwise        = []
count (l:ls) ds
    | l == 2 || l == 5 = map (l:) (count ls ds)
    | otherwise        = do
        (ds1,ds2) <- partitions ds
        guard $ l == 3 && null ds1 || ((l + sum ds1) `mod` 3 /= 0)
        ds1p <- permutations ds1
        let n1 = N.foldDigits (l:ds1p)
        guard $ N.isPrimeBy primes n1
        ns <- count ls ds2
        return $ n1:ns

solution :: [[Int]]
solution = do
    (ls,ds') <- partitions [1,2,3,5,7,9]
    ns <- count ls (merge ds' [4,6,8])
    return ns

main = print $ length solution