module Problem365 where

import Data.Array
import Data.Numbers.Primes

-- powPinFact (prime p) n = k where k is maximal s.t. p^k divides n!
powPinFact :: Integer -> Integer -> Integer
powPinFact 0 p = 0
powPinFact n p = let m = n `div` p in m + powPinFact m p

-- powMod x e n = x^e mod n
powMod :: Integer -> Integer -> Integer -> Integer
powMod x 0 n = 1
powMod x e n = if r == 0 then (y*y) `mod` n else (x*y*y) `mod` n
  where (q,r) = e `divMod` 2
        y = powMod x q n

-- powModPrime x n (prime p) = x^n mod p
powModP :: Integer -> Integer -> Integer -> Integer
powModP x e p = powMod x (e `mod` (p-1)) p

-- factModP n (prime p) = (n! / k) mod p where k is maximal s.t. p^k divies n!
factModP :: Integer -> Integer -> Integer
factModP n p = run n
    where partProdModP = listArray (0,p-1) (scanl (\a b -> (a*b) `mod` p) 1 [1..p-1])
          run k = let (q,r) = k `divMod` p
                      s = if even q then 1 else -1
                  in  if q == 0 then partProdModP ! r else (s * partProdModP ! r * run q) `mod` p

factModP_spec :: Integer -> Integer -> Integer
factModP_spec n p = stripPs (product [1..n]) `mod` p
    where stripPs k = case k `divMod` p of
                        (q,0) -> stripPs q
                        (q,r) -> k

-- euclid a b = (gcd(a,b),r,s) where r*a + s*b = gcd(a,b)
euclid :: Integer -> Integer -> (Integer,Integer,Integer)
euclid a b
    | c == 0    = (b,0,1)
    | otherwise = let (d,r,s) = euclid b c
                  in  (d,s,r-s*q)
    where (q,c) = a `divMod` b

-- invMod a n = a^(-1) mod n
invMod :: Integer -> Integer -> Integer
invMod a n = let (1,r,s) = euclid (a `mod` n) n in r `mod` n

-- binomModP n k (prime p) = C(n,k) mod p
binomModP :: Integer -> Integer -> Integer -> Integer
binomModP n k p
    | pn > pk+pl = 0
    | otherwise  = (factModP n p * invMod (factModP k p * factModP l p) p) `mod` p
    where l = n-k
          pn = powPinFact n p
          pk = powPinFact k p
          pl = powPinFact l p

binomModP_spec :: Integer -> Integer -> Integer -> Integer
binomModP_spec n k p = binom n k `mod` p

-- binom n k = C(n,k)
binom :: Integer -> Integer -> Integer
binom n k = foldl (\r (a,b) -> (r*a) `div` b) 1 $ zip [n,n-1..] [1..k]

chinese :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
chinese (a,m) (b,n) = let (1,r,s) = euclid m n
                      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)

tuples :: Int -> [a] -> [[a]]
tuples 1 xs     = map (:[]) xs
tuples r []     = []
tuples r (x:xs) = map (x:) (tuples (r-1) xs) ++ tuples r xs

solution = sum . map (fst . foldl1 chinese) . tuples 3 . map (\p -> (binomModP (10^18) (10^9) p,p)) . takeWhile (< 5000) . dropWhile (<= 1000) $ primes

main = print solution