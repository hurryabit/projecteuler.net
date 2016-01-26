module Problem504 where

import Data.Array

isSquare :: Int -> Bool
isSquare n =
    let r = floor . sqrt $ fromIntegral n
    in  n == r*r

table :: Int -> Array (Int,Int) Int
table m =
    let bnds = ((1,1),(m,m))
        f (a,b) = sum [ ((a-x)*b-1) `div` a | x <- [1 .. a-1] ]
    in  listArray bnds $ map f (range bnds)

solution :: Int -> Int
solution m = length $ filter isSquare
    [ t ! (a,b) + t ! (a,d) + t ! (c,b) + t ! (c,d) + a + b + c + d - 3
    | a <- [1..m], b <- [1..m], c <- [1..m], d <- [1..m]
    ]
    where t = table m

main = print $ solution 100