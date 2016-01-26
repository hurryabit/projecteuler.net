module Problem76 where

import Control.Monad
import Data.Array

sols :: Array (Int,Int) Int
sols = listArray size $ map f $ range size
  where m = 10000
        size = ((0,1),(m,m))
        f (0,k) = 1
        f (n,k) = sum [ sols ! (n-i,i) | i <- [k..n] ]
