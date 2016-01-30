module Problem164 where

import Data.Array

solution = sum [ table ! (0,a,19) | a <- [1..9] ]
  where table :: Array (Int,Int,Int) Integer
        table = listArray dim (map f (range dim))
        dim = ((0,0,0),(9,9,19))
        f (_,_,0) = 1
        f (a,b,k) = sum [ table ! (b,c,k-1) | c <- [0..9-a-b] ]
