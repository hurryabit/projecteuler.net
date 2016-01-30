module Problem95 where

import Data.Array.IArray
import Data.List
import Data.Ord
import qualified Data.Set as Set
import Numbers

divisorsSumArray :: Array Int Int
divisorsSumArray = amap (sum . init) $ divisorsArray 999999

divisorsSum :: Int -> Int
divisorsSum n
  | 1 <= n && n <= 999999 = divisorsSumArray ! n
  | otherwise             = 0

chain :: Int -> [Int]
chain = takeWhile (/= 0) . iterate divisorsSum

uniquePrefix :: Ord a => [a] -> ([a],Maybe a)
uniquePrefix = run Set.empty
  where
    run s [] = ([],Nothing)
    run s (x:xs)
      | x `Set.member` s = ([],Just x)
      | otherwise        = let (ys,r) = run (x `Set.insert` s) xs in (x:ys,r)

amicable :: Int -> Int
amicable n
  | r == Just n = length ys
  | otherwise   = 0
  where (ys,r) = uniquePrefix $ chain n

runningMaximumBy :: (a -> a -> Ordering) -> [a] -> [a]
runningMaximumBy cmp [] = []
runningMaximumBy cmp (x:xs) =
  x:runningMaximumBy cmp (dropWhile ((LT /=) . cmp x) xs)

solution =
  runningMaximumBy (comparing snd) [ (k,amicable k) | k <- [1..999999] ]

main = mapM_ print solution
