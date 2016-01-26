module Problem483b where

import qualified Data.Map.Strict as Map
import Data.MemoCombinators
import Data.Ratio

type Counter = Double

orders :: (Integer,Integer) -> [(Integer,Counter)]
orders = arrayRange ((0,0),(350,350)) orders'

--solve' :: (Integer,Integer) -> Integer
orders' (0,0)   = [(1,1)]
orders' (n,0)   = []
orders' (n,k)
    | n < k     = orders (n,n)
    | otherwise = collect $ do
        r <- [0 .. n `div` k]
        let d = recip . fromInteger $ k^r * product [1 .. r]
        (l,c) <- orders (n-r*k,k-1)
        return (if r > 0 then lcm k l else l,c*d)

collect :: [(Integer,Counter)] -> [(Integer,Counter)]
collect = Map.toList . Map.fromListWith (+)

solve :: Integer -> Double
solve n = sum [ c * (fromInteger l)^2 | (l,c) <- orders (n,n) ]

main = print $ solve 150

partitions :: Integer -> Integer -> [(Integer,[(Integer,Integer)])]
partitions 0 0 = [(1,[])]
partitions n 0 = []
partitions n k = do
    r <- [0 .. n `div` k]
    (c,krs) <- partitions (n-r*k) (k-1)
    return $ ((c*product [n-r*k+1 .. n]) `div` (product [1..r] * k^r),(k,r):krs)

solveSpec :: Integer -> Integer
solveSpec n = sum $ do
    (c,krs) <- partitions n n
    let ks = [ k | (k,r) <- krs, r > 0 ]
    return $ c * (foldr1 lcm ks)^2

--countP :: Int -> [(Int,Int)] -> Int
--countP n = product . map (\(k,r) -> product [n-r*k+1])