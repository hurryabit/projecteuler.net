module Problem126 where

import Control.Monad
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

layers :: (Int,Int,Int) -> [Int]
layers (a,b,c) = take 5 [ 2*(a*b+a*c+b*c) + (n-1)*4*(a+b+c) + (n-1)*(n-2)*4 | n <- [1..] ]

triples :: Int -> [(Int,Int,Int)]
triples n = [ (a,b,n-(a+b)) | a <- [1..n `div` 3], b <- [a..(n-a) `div` 2] ]

merge :: Ord a => [[a]] -> [a]
--merge ([]:xss)     = merge xss
merge ([x]:xss)    = x:merge xss
merge ((x:xs):xss) = x:merge (insertBy (comparing head) xs xss)

--solution :: Int -> Int
solution n =  head . head . filter ((n ==) . length) . group . merge . merge . map (sort . map layers . triples) $ [3..]

main = print $ solution 100

-- Specification

specLayers :: (Int,Int,Int) -> [Int]
specLayers dim = let sizes = map Set.size (cuboids dim)
                   in  zipWith (-) (tail sizes) sizes

cuboids :: (Int,Int,Int) -> [Set (Int,Int,Int)]
cuboids (a,b,c) = iterate step start
    where start = Set.fromList $ liftM3 (,,) [1..a] [1..b] [1..c]
          step cub = Set.fromList $ concat [ [(x,y,z),(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)] | (x,y,z) <- Set.toList cub ]
