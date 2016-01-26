module Problem33 where

import Data.Ratio

sols = [ x%y | x <- [1..9], y <- [x+1..9], a <- [1..9],
  (10*a+x)*y==(10*y+a)*x || (10*x+a)*y==(10*a+y)*x ]
