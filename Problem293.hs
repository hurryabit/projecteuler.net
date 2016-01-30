module Problem293 where

import qualified Numbers

main = print solution

solution = sum . unique . map fortunateOf $ takeWhile (<1000000000) admissibles

primes :: [Integer]
primes = Numbers.primes

admissiblesBy :: [Integer] -> [Integer]
admissiblesBy (p:ps) = merge [ map (*k) as | k <- iterate (*p) p ]
  where as = 1:admissiblesBy ps

admissibles :: [Integer]
admissibles = admissiblesBy primes

fortunateOf :: Integer -> Integer
fortunateOf n = (head . dropWhile (not . Numbers.isPrimeBy primes) . map (n+) $ [3,5..]) - n

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter (/=x) xs)

merge :: Ord a => [[a]] -> [a]
merge ((x:xs):xss) = x:merge (insert xs xss)
  where insert (ys@(y:_)) (zs@(z:_):zss)
          | y <= z    = ys:zs:zss
          | otherwise = zs:insert ys zss
