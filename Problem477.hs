module Problem477 where

import Data.Array
import Data.MemoCombinators (memo2, unsafeArrayRange)

random :: [Integer]
random = iterate (\x -> (x*x+45) `mod` 1000000007) 0

en :: Int
en = 1000

board :: Array Int Integer
board = listArray (0,en-1) random

main = print $ table ! (0,en-1)

table :: Array (Int,Int) Integer
table = array ((0,0),(en-1,en-1)) [ ((l,r),solver l r) | d <- [0 .. en-1], l <- [0 .. en-1-d], let r = l+d ]

psums :: Array (Int,Int) (Integer,Integer)
psums = array ((0,0),(en-1,en-1)) [ ((l,r),p l r) | l <- [en-1,en-2 .. 0], r <- [l .. en-1] ]
    where p l r = case l `compare` r of
                    EQ -> (board ! l,0)
                    LT -> let (x,y) = psums ! (l+1,r)
                          in  (y + board ! l,x)

guaranteed :: Int -> Int -> Integer
guaranteed l r = uncurry max $ psums ! (l,r)

solver :: Int -> Int -> Integer
solver = approx
--solver = exact

approx :: Int -> Int -> Integer
approx l r = case l `compare` r of
    GT              -> error "solve: l > r"
    EQ              -> 0
    LT | even (l+r) -> min (table ! (l+1,r)) (table ! (l,r-1))
       | otherwise  ->
           let (ev,od) = psums ! (l,r)
           in  max (board ! l + min (guaranteed (l+2) r) (guaranteed (l+1) (r-1)))
                   (board ! r + min (guaranteed (l+1) (r-1)) (guaranteed l (r-2)))

exact :: Int -> Int -> Integer
exact l r = case l `compare` r of
    GT              -> error "solve: l > r"
    EQ              -> 0
    LT | even (l+r) -> min (table ! (l+1,r)) (table ! (l,r-1))
       | otherwise  -> max (table ! (l+1,r) + board ! l) (table ! (l,r-1) + board ! r)
