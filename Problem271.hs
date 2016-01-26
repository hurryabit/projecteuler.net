module Problem271 where

import Control.Monad

euclid :: Integral a => a -> a -> (a,a,a)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

chinese :: Integral a => (a,a) -> (a,a) -> (a,a)
chinese (a,m) (b,n) = let (1,r,s) = euclid m n
                      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)

roots :: Integral a => Int -> (a,a) -> [(a,a)]
roots e (a,m) = [ (b,m) | b <- [0..m-1], b^e `mod` m == a ]

cartesian :: [[a]] -> [[a]]
cartesian [xs] = map (:[]) xs
cartesian (xs:xxs) = liftM2 (:) xs (cartesian xxs)

solution :: Integer
solution = sum . map (fst . foldl1 chinese) . tail . cartesian . map (roots 3 . (,) 1) $ [2,3,5,7,11,13,17,19,23,29,31,37,41,43]