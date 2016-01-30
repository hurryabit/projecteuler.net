module Tuples where

import Data.List

pairs :: [a] -> [(a,a)]
pairs xs = concat $ zipWith (map . (,)) xs (tails (tail xs))

triples :: [a] -> [(a,a,a)]
triples xs = concat $ zipWith (\y -> map (uncurry ((,,) y)) . pairs) xs (tails (tail xs))

tuples :: Int -> [a] -> [[a]]
tuples 1 xs = map (:[]) xs
tuples n xs = concat $ zipWith (\y -> map (y:) . tuples (n-1)) xs (tails (tail xs))
