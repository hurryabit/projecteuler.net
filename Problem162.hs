{-# LANGUAGE NoMonomorphismRestriction #-}
module Problem162 where

import Text.Printf

solution = printf "%X\n" (sum [(if p==1 then 4 else 6)*13^(p-1)*14^(q-p-1)*15^(r-q-1)*16^(k-r) | k <- [3..16], p <- [1..k-2], q <- [p+1..k-1], r <- [q+1..k]] :: Integer)
