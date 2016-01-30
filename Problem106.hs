module Problem106 where

choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

doubleChoose :: Ord a => Int -> [a] -> [([a],[a])]
doubleChoose k xs =
  [ (ys,zs) | ys <- choose k xs
            , zs <- choose k (filter (`notElem` ys) xs)
            , head ys < head zs
            , or $ zipWith (>) ys zs ]
