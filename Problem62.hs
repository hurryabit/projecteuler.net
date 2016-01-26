module Problem62 where

import Data.List
import qualified Data.Map as Map
import Numbers

nf :: Integer -> Integer
nf = foldDigits . sortBy (flip compare) . digits

count :: Ord a => [a] -> [(a,Int)]
count xs = Map.toList . Map.fromListWith (+) . zip xs . repeat $ 1

counts :: [(Integer,Int)]
counts = concatMap (count . map nf) cubes

cubes :: [[Integer]]
cubes = run 1 (map (^3) [1..])
  where run n xs = let  n' = 10*n
                        (ys,zs) = span (<n') xs
                   in   ys:run n' zs

main = print $ find ((5==) . snd) counts
