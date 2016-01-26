module Problem483a where

import Data.Array
import qualified Data.IntMap.Strict as Map
import Data.MemoCombinators
import Data.Numbers.Primes
import Data.Ratio

type Counter = Double

small p = p*p < 350

bigPrimes :: Array Int Int
bigPrimes = let ps = takeWhile (<350) $ dropWhile small primes
            in  listArray (0,length ps) (0:ps)

composites :: Array Int Int
composites = let cs = filter (all small . primeFactors) [1 .. 350]
             in  listArray (0,length cs) (0:cs)

funG :: Int -> Int -> Int -> Double
funG = memo3 (arrayRange (0,350)) (arrayRange $ bounds bigPrimes) bits funG'

funG' n 0  l = sum [ (fromIntegral $ lcm l k)^2 * t | (k,t) <- orders n (snd $ bounds composites) ]
funG' n ip l = funG n (ip-1) l + sum (map f $ partitions (n `div` p))
    where
    p = bigPrimes ! ip
    f krs = let d = product [ product [1 .. fromIntegral r] * (fromIntegral (k*p))^r | (k,r) <- krs ]
                s = sum [ k*r | (k,r) <- krs ]
                l' = foldr lcm l (map fst krs)
            in  (fromIntegral p)^2 * funG (n-p*s) (ip-1) l' / d

orders :: Int -> Int -> [(Int,Double)]
orders = memo2 (arrayRange (0,350)) (arrayRange $ bounds composites) orders'

orders' 0 0     = [(1,1.0)]
orders' n 0     = []
orders' n ic
    | n < c     = orders n (ic-1)
    | otherwise = collect $ orders n (ic-1) ++ concatMap f [1 .. n `div` c]
    where
    c = composites ! ic
    f r = let d = product [1 .. fromIntegral r] * (fromIntegral c)^r
          in  [ (lcm c l,t/d) | (l,t) <- orders (n-r*c) (ic-1) ]

collect :: [(Int,Double)] -> [(Int,Double)]
collect = Map.toList . Map.fromListWith (+)

solve :: Int -> Double
solve n = funG n (snd $ bounds bigPrimes) 1

main = print $ solve 150

partitions :: Int -> [[(Int,Int)]]
partitions n = tail $ partitions' n n

partitions' :: Int -> Int -> [[(Int,Int)]]
partitions' n 0 = [[]]
partitions' n k = partitions' n (k-1) ++ [ (k,r):krs | r <- [1 .. n `div` k], krs <- partitions' (n-r*k) (k-1) ]
