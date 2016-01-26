module Problem160 where

fun1 :: Integer -> Integer -> Integer -> [(Integer,Integer)]
fun1 0 p k = []
fun1 n p k = let p2k = p^k
                 phi_p2k = (p-1)*p^(k-1)
                 (q,r) = n `divMod` p2k
                 s1 = powMod (product [ i | i <- [1..p2k], i `mod` p /= 0 ] `mod` p2k) (q `mod` phi_p2k) p2k
                 s2 = product [ j | j <- [1..r], j `mod` p /= 0 ] `mod` p2k
                 n' = n `div` p
             in  ((s1*s2) `mod` p2k, n'):fun1 n' p k

fun2 :: Integer -> Integer -> Integer -> (Integer,Integer)
fun2 n p k = foldl1 (\(s1,l1) (s2,l2) -> ((s1*s2) `mod` (p^k),l1+l2))$ fun1 n p k

powMod :: Integer -> Integer -> Integer -> Integer
powMod x 0 n = 1
powMod x e n = if r == 0 then (y*y) `mod` n else (x*y*y) `mod` n
  where (q,r) = e `divMod` 2
        y = powMod x q n

solution :: Integer -> Integer
solution n = let f2f = 5^5
                 (s,l) = fun2 n 5 5
                 r = invMod (powMod 2 l f2f) f2f
                 (r10,_) = chinese (0,2^5) (s*r,5^5)
             in  r10    

euclid :: Integer -> Integer -> (Integer,Integer,Integer)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

invMod :: Integer -> Integer -> Integer
invMod a n = let (1,r,s) = euclid (a `mod` n) n in r `mod` n

chinese :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
chinese (a,m) (b,n) = let (1,r,s) = euclid m n
                      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)
