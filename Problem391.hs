module Problem391 where

import Data.Bits

next :: Int -> Int -> Maybe Int
next n x
    | x <= 0    = Nothing
    | b <= n    = next (n-b) (x-1)
    | otherwise = Just $ x-1
    where b = popCount x

start :: Int -> Int
start n = case run (2^(n+1) - 1) of
    Just x  -> sum $ map popCount [1..x]
    Nothing -> 0
    where run :: Int -> Maybe Int
          run x = case next n x of
                    Nothing -> Just x
                    Just 0  -> Nothing
                    Just y  -> run y

main = print $ sum [ (start n)^3 | n <- [1..20] ]