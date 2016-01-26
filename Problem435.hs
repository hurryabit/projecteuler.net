module Problem435 where

import Data.List
import qualified Data.Set as Set

fibsMod :: Integer -> [Integer]
fibsMod m = let fs = 0:1:(map (\(a,b) -> (a+b) `mod` m) $ takeWhile ((m-1,1) /= ) $ zip fs (tail fs))
            in  fs

detectCycle :: Ord a => [a] -> ([a],[a])
detectCycle xs =
    let (_,xbs)       = mapAccumL (\s x -> (Set.insert x s,(x,x `Set.member` s))) Set.empty xs
        (ybs,(z,_):_) = break snd xbs
    in  break (z ==) (map fst ybs)

powersMod :: Integer -> Integer -> ([Integer],[Integer])
powersMod m x = detectCycle $ iterate (\y -> (x*y) `mod` m) 1

scalarMod :: Integer -> [Integer] -> [Integer] -> Integer
scalarMod m xs ys = sum (zipWith (*) xs ys) `mod` m

unroll :: Int -> [a] -> ([a],[a])
unroll l xs =
    let (ys,zs) = splitAt l xs
    in  (ys,zs++ys)

eff :: Integer -> Integer
eff x =
    let fs = 0:1:zipWith (+) fs (tail fs)
        xs = iterate (*x) 1
    in  sum $ zipWith (*) fs (take (degree+1) xs)

degree = 10^15

effMod :: Integer -> Integer -> Integer
effMod m x =
    let fs         = fibsMod m
        (xs1,xs2)  = powersMod m x
        (fs1,fs2)  = unroll (length xs1) fs
        lf         = length fs2
        lx         = length xs2
        lc         = lcm lf lx
        fs3        = concat $ replicate (lc `div` lf) fs2
        xs3        = concat $ replicate (lc `div` lx) xs2
        (q,r)      = (degree+1 - length xs1) `divMod` lc
    in  (scalarMod m fs1 xs1 + (toInteger q `mod` m) * scalarMod m fs3 xs3 + scalarMod m fs3 (take r xs3)) `mod` m

effModChinese :: [Integer] -> Integer -> Integer
effModChinese ms x = fst . chinese $ map (\m -> (effMod m x,m)) ms

modulus = [2^11,3^6,5^3,7^2,11,13]

checkModulus :: [Integer] -> [Integer]
checkModulus ms =
    let m = product ms
    in  [ x | x <- [0 .. 2*m], effModChinese ms x /= eff x `mod` m ]

solution = sum (map (effModChinese modulus) [0 .. 100]) `mod` product modulus

main = print solution

euclid :: Integer -> Integer -> (Integer,Integer,Integer)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

chinese2 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
chinese2 (a,m) (b,n) = let (1,r,s) = euclid m n
                       in  ((s*n*a+r*m*b) `mod` (m*n),m*n)

chinese :: [(Integer,Integer)] -> (Integer,Integer)
chinese = foldr1 chinese2