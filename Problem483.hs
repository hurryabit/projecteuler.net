module Problem483 where

import Control.Monad
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

smallPrimes :: [Int]
smallPrimes = takeWhile small primes

l2i :: Map.IntMap Int
l2i = snd $ Map.mapAccum (\i _ -> (i+1,i)) 1 $ Map.fromList [ (l,()) | l <- lcms' smallPrimes ]
    where
    lcms' :: [Int] -> [Int]
    lcms' []     = [1]
    lcms' (p:ps) = liftM2 (*) (takeWhile (< 350) $ iterate (p*) 1) (lcms' ps)

i2l :: Array Int Int
i2l = array (1,Map.size l2i) [ (i,l) | (l,i) <- Map.assocs l2i ]

composites :: Array Int Int
composites = let cs = filter (all small . primeFactors) [1 .. 350]
             in  listArray (0,length cs) (0:cs)

funG :: Int -> Int -> Int -> Double
funG = memo3 (arrayRange (0,350)) (arrayRange $ bounds bigPrimes) bits funG'

funG' n 0  l = sum [ (fromIntegral $ lcm l k)^2 * t | (k,t) <- assocs $ orders n (snd $ bounds composites) ]
funG' n ip l = funG n (ip-1) l + sum (map f $ partitions (n `div` p))
    where
    p = bigPrimes ! ip
    f krs = let d = product [ product [1 .. fromIntegral r] * (fromIntegral (k*p))^r | (k,r) <- krs ]
                s = sum [ k*r | (k,r) <- krs ]
                l' = foldr lcm l (map fst krs)
            in  (fromIntegral p)^2 * funG (n-p*s) (ip-1) l' / d

orders :: Int -> Int -> Array Int Double
orders = memo2 (arrayRange (0,350)) (arrayRange $ bounds composites) orders'

orders' 0 0     = accumArray (+) 0 (bounds i2l) [(1,1.0)]
orders' n 0     = accumArray (+) 0 (bounds i2l) []
orders' n ic
    | n < c     = orders n (ic-1)
    | otherwise = accum (+) (orders n (ic-1)) $ concatMap f [1 .. n `div` c]
    where
    c = composites ! ic
    f r = do
            let d = product [1 .. fromIntegral r] * (fromIntegral c)^r
            (il,t) <- assocs (orders (n-r*c) (ic-1))
            let l = i2l ! il
                k = lcm c l
                Just ik = Map.lookup k l2i
            return (ik,t/d)

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
