module Problem209 where

import Control.Applicative
import Data.List

type Node = (Bool,Bool,Bool,Bool,Bool,Bool)

nodes :: [Node]
nodes = (,,,,,) <$> z <*> z <*> z <*> z <*> z <*> z
    where z = [False,True]

next :: Node -> Node
next (a,b,c,d,e,f) = (b,c,d,e,f,a /= (b && c))

solution :: Int
solution = product $ map (cycleCount !!) decompose

pathCount :: [Int]
pathCount = let xs = 1:2:zipWith (+) xs (tail xs) in xs

cycleCount :: [Int]
cycleCount = let xs = pathCount in 1:1:3:zipWith (+) xs (drop 2 xs)

decompose :: [Int]
decompose = unfoldr firstCycle nodes

firstCycle :: [Node] -> Maybe (Int,[Node])
firstCycle []       = Nothing
firstCycle xs@(x:_) = Just (length ys,foldr delete xs ys)
    where ys = x:takeWhile (x /=) (tail (iterate next x))