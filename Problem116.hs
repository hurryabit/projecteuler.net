module Problem116 where

import Data.Function

sol = let f n = fix $ \xs -> replicate n 1 ++ zipWith (+) xs (drop (n-1) xs)
      in  (zipWith3 (\x y z -> x+y+z-3) (f 2) (f 3) (f 4)) !! 50
