module Problem182 where

solution :: Int
solution = sum $ takeWhile (< 3675787) [ i*50988+r | i <- [0..], r <- r50988 ]
    where r50988 = [ fst $ chinese (11,12) $ chinese (r7,7) (r607,607)
                   | r7   <- [2..6]
                   , r607 <- [2..606]
                   ]

main = print solution

euclid :: Int -> Int -> (Int,Int,Int)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

chinese :: (Int,Int) -> (Int,Int) -> (Int,Int)
chinese (a,m) (b,n) = let (1,r,s) = euclid m n
                      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)
