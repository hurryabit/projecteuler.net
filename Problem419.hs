module Problem419 where

import Data.List

step :: [Int] -> [Int]
step xs = concatMap (\ys@(y:_) -> [length ys,y]) (group xs)

las :: [[Int]]
las = iterate step [1]

lasA, lasB, lasC :: [Int]
lasA = lasX 1
lasB = lasX 2
lasC = lasX 3

lasX :: Int -> [Int]
lasX x = map (length . filter (x == )) las