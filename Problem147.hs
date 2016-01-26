module Problem147 where

import Control.Applicative
import Data.MemoCombinators

count :: Int -> Int -> Int
count = memo2 (arrayRange (0,47)) (arrayRange (0,47)) count'

count' x y
    | x < y     = count y x
    | x == 0    = 0
    | x == 1    = y
    | otherwise = let skws = case x-y of
                                0 -> 8*y-9
                                1 -> y
                                _ -> 0
                  in  2 * count (x-1) y - count (x-2) y + y*(y+1) `div` 2 + skws

solution = sum $ count <$> [1..47] <*> [1..43]