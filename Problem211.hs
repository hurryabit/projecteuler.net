module Problem211 where

import Data.List
import Data.Numbers.Primes

main = print $ solution 64000000

solution :: Integer -> Integer
solution lim = search (takeWhile (< (lim `div` 2)) primes) 1 1 0
    where search :: [Integer] -> Integer -> Integer -> Integer -> Integer
          search (p:ps) n sig res
              | p*n < lim = foldl' f res $ takeWhile ((lim >) . fst) $ iterate (\(n',d2) -> (p*n',p*p*d2+1)) (n,1)
              where f res' (n',d2) = search ps n' (d2*sig) res'
          search _      n sig res
              | isSquare sig = res+n
              | otherwise    = res

isSquare :: Integer -> Bool
isSquare n = let r = floor $ sqrt $ fromIntegral n in n == r*r
