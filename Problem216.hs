module Problem216 where

import Control.Monad
import Data.List
import Data.Numbers.Primes (wheelSieve)

primes :: [Int]
primes = wheelSieve 6

isPrime :: Int -> Bool
isPrime n = check n primes
    where
    check m (p:ps)
        | m < p*p        = True
        | m `mod` p == 0 = False
        | otherwise      = check m ps

solution m = length $ filter isPrime [ 2*n*n-1 | n <- [2 .. m] ]

--main = print $ solution 1000000#
main = print $ length $ takeWhile (< 71000000) primes

euclid :: Integer -> Integer -> (Integer,Integer,Integer)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

invMod :: Integer -> Integer -> Integer
invMod m a = let (1,r,s) = euclid a m in r

powMod :: Integer -> Integer -> Integer -> Integer
powMod m x 0 = 1
powMod m x e = if r == 0 then (y*y) `mod` m else (x*y*y) `mod` m
  where (q,r) = e `divMod` 2
        y = powMod m x q

legendre :: Integer -> Integer -> Integer
legendre p a = powMod p a (p `div` 2)

twos :: Integer -> (Integer,Integer)
twos n
    | even n    = let (k,m) = twos (n `div` 2) in (k+1,m)
    | otherwise = (0,n)

sqrtModP :: Integer -> Integer -> Integer -> Maybe Integer
sqrtModP p n z
    | legendre p n /= 1 = Nothing
    | s == 1            = Just $ powMod p n ((p+1) `div` 4)
    | otherwise         =
        let --Just z = find (((p-1) ==) . legendre p) [1..p-1]
            --z = 
            step (r,t,m,c) =
                let Just i = find (\i -> powMod p t (2^i) == 1) [1 .. m-1]
                    b = powMod p c (2^(m-i-1))
                in  ((r*b) `mod` p,(t*b*b) `mod` p,i,(b*b) `mod` p)
            Just (r,_,_,_) = find (\(_,t,_,_) -> t == 1) $ iterate step (powMod p n ((q+1) `div` 2),powMod p n q,s,powMod p z q)
        in  Just r
    where
    (s,q) = twos (p-1)

step p (r,t,m,c) =
                let Just i = find (\i -> powMod p t (2^i) == 1) [1 .. m-1]
                    b = powMod p c (2^(m-i-1))
                in  ((r*b) `mod` p,(t*b*b) `mod` p,i,(b*b) `mod` p)


{-checkSqrtModP :: Integer -> [Integer]
checkSqrtModP p = do
    n <- [1 .. p-1]
    case sqrtModP p n of
        Nothing -> mzero
        Just r  -> guard ((r*r) `mod` p /= n) >> return p-}