module Problem287 where

import Data.Array
import Data.List
import GHC.Prim

dim = 24
rad = 2^(dim-1)
rad2 = square rad

{-# INLINE square #-}
square :: Int -> Int
square n = n*n

{-# NOINLINE pow2 #-}
pow2 :: Array Int Int
pow2 = listArray (0,24) $ take 25 $ iterate (2*) 1

type Point = (Int,Int)

{-# INLINE black #-}
black :: Point -> Bool
black (x,y) = square (x-rad) + square (y-rad) <= rad2

type Region = (Int,Int,Int)

{-# INLINE divide #-}
divide :: Region -> [Region]
divide (rd,rx,ry) = [ (rd+1,2*rx+dx,2*ry+dy) | dy <- [1,0], dx <- [0,1] ]

{-# INLINE corners #-}
corners :: Region -> [Point]
corners (rd,rx,ry) = [ (rx*l+dx,ry*l+dy) | dx <- [0,l-1], dy <- [0,l-1] ]
  where l = pow2 ! (dim-rd)

encoding :: Int -> Region -> Int
encoding cnt r@(rd,_,_)
  | rd /= 0 && (null bs || null ws) = cnt+2
  | otherwise                       = foldl' (\cnt' r' -> encoding cnt' r') (cnt+1) (divide r)
  where (bs,ws) = partition black (corners r)

main = print $ encoding 0 (0,0,0)
