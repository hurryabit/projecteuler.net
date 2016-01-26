module Problem133 where

import qualified Data.Set as Set

import qualified Numbers as N

main = do
  let sols = solutions
  mapM_ print sols
  print (sum sols)

solutions :: [Integer]
solutions =
  2:3:5:filter (not . canBeFactor) (takeWhile (<100000) . drop 3 $ N.primes)

canBeFactor :: Integer -> Bool
canBeFactor p = any (\k -> powMod 10 k p == 1) (remainders 10 (p-1))

-- powMod x n m = x^n mod m
powMod :: Integer -> Integer -> Integer -> Integer
powMod x 0 m = 1
powMod x n m = if e == 0 then y2 else (x*y2) `mod` m
  where (k,e) = n `divMod` 2
        y = powMod x k m
        y2 = (y*y) `mod` m

-- powModPrime x n p = x^n mod p where p is a prime
powModPrime :: Integer -> Integer -> Integer -> Integer
powModPrime x n p = powMod x (n `mod` (p-1)) p

-- remainders x m = [x mod m, x^2 mod m, x^3 mod m, ..., x^(k-1) mod m]
--   where k is the smallest number for which an i < k exists with
--   x^i mod m = x^k mod m
remainders :: Integer -> Integer -> [Integer]
remainders x m = distinct $ iterate (\y -> (x'*y) `mod` m) x'
  where x' = x `mod` m

distinct :: Ord a => [a] -> [a]
distinct = run Set.empty
  where run s [] = []
        run s (x:xs)
          | x `Set.member` s = []
          | otherwise        = x:run (x `Set.insert` s) xs
