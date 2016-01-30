module Problem234 where

import Numbers

semiDivisibles :: [Integer]
semiDivisibles = concat $ zipWith semiDivisiblesBetween ps (tail ps)
  where ps = primes

semiDivisiblesBetween :: Integer -> Integer -> [Integer]
semiDivisiblesBetween p q = merge [p2+p,p2+2*p..q2-1] [q0,q0+q..q2-1]
  where p2 = p*p
        q2 = q*q
        q0 = (p2 `div` q + 1)*q

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = case compare x y of
  LT -> x:merge xs (y:ys)
  EQ -> merge xs ys
  GT -> y:merge (x:xs) ys

main = do
  let sol = sum . takeWhile (<= 999966663333) $ semiDivisibles
  print sol
