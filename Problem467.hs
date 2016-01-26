{-# LANGUAGE BangPatterns #-}
module Problem467 where

import Control.Monad
import Data.List
import Data.Numbers.Primes

digitRoot :: Int -> Int
digitRoot = until (< 10) digitSum
    where
    digitSum = sum . unfoldr (\n -> guard (n > 0) >> return (uncurry (flip (,)) $ n `divMod` 10))

composites :: [Int]
composites = concat $ zipWith (\p q -> [p+1 .. q-1]) primes (tail primes)

pd, cd :: [Int]
pd = map digitRoot primes
cd = map digitRoot composites

scsup :: Ord a => [a] -> [a] -> [a]
scsup xs = snd . last . foldr step (zip [0..] (reverse $ tails xs))
    where
    xsrev = reverse xs
    step y ass@((las0,as0):ass') = let sext x (lbs,bs) (las',as') (las,as)
                                          | x == y    = (las+1,x:as)
                                          | otherwise = min (las'+1,y:as') (lbs+1,x:bs)
                                       bss = (las0+1,y:as0):zipWith4 sext xsrev bss ass' ass
                                   in  bss


scsupint :: [Int] -> [Int] -> Int
scsupint xs = fourth . last . foldr step (reverse $  scanr push (0,0,1,0) xs)
    where
    fourth (_,_,_,d) = d
    mod1g7 n
        | n < 1000000007 = n
        | otherwise      = n `mod` 1000000007
    push d (!m,!l,!m',!n) = (m+1,d,mod1g7 (10*m'),mod1g7 (d*m'+n))
    xsrev = reverse xs
    step y ass@(as0:ass') = let sext x bs as' as
                                  | x == y    = push x as
                                  | otherwise = min (push y as') (push x bs)
                                bss = (push y as0):zipWith4 sext xsrev bss ass' ass
                            in  bss


solve :: Int -> Int
solve n = scsupint (take n pd) (take n cd)

main = print $ solve 1000
