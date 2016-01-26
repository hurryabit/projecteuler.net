module Problem452 where

prods :: Int -> Int -> [[Int]]
prods 0 _ = [[]]
prods l s = [ k:ks | k <- [1..s], ks <- prods (l-1) (s `div` k)]