module Problem493 where

import Control.Monad
import Data.Array
import Data.List
import Data.Ratio

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs):[ (y,x:ys) | (y,ys) <- select xs ]

pack :: [Int] -> Int
pack = foldr (\d n -> 11*n+d) 0

unpack :: Int -> [Int]
unpack = unfoldr f 
    where
        f n = do
            guard $ n > 0
            let (q,r) = n `divMod` 11
            return (r,q)

start :: Int
start = pack (replicate 7 10)

table :: Array Int Double
table =
    let bnds = (pack (replicate 6 7 ++ [8]), start)
    in  listArray bnds (map func $ range bnds)

func :: Int -> Double
func n
    | s == 50   = fromIntegral $ length $ filter (< 10) ks
    | otherwise = sum $ do
        (k,ks') <- select ks
        guard $ k > 0
        return $ fromIntegral k * table ! pack (insert (k-1) ks') / fromIntegral s
    where
        ks = unpack n
        s  = sum ks

main = print $ table ! start