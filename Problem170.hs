module Problem170 where

import Control.Arrow
import Data.Array

pandigital :: String -> Bool
pandigital ds = all (1 ==) . elems $ accumArray (+) 0 ('0','9') [ (d,1) | d <- ds ]

select :: [a] -> [(a,[a])]
select [] = []
select (d:ds) = (d,ds):map (second (d:)) (select ds)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations ds = concatMap (\(d,ds) -> map (d:) $ permutations ds) $ select ds

splits :: [a] -> [([a],[a])]
splits []     = error "split: empty list"
splits [_]    = error "split: singleton list"
splits [d,e]  = [([d],[e])]
splits (d:ds) = ([d],ds):map (first (d:)) (splits ds)

solve = do
    ds <- permutations ['9','8' .. '1']
    (es,fs) <- splits ds