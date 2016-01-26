module Problem327 where

funM :: Int -> Int -> Int
funM c 0 = 1
funM c r
    | m < c     = m + 1
    | otherwise = 2*((m-c) `div` (c-2) + 1) + 1 + m
    where m = funM c (r-1)

solution :: Int
solution = sum [ funM c 30 | c <- [3..40] ]