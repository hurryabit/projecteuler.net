module Problem484 where

import Data.Numbers.Primes

main = print $ solve (5*10^15)

solve :: Int -> Int
solve n = work n (map f primes) - 1
    where f p = let gs = scanl (*) 1 . cycle $ replicate (p-2) p ++ [p*p,1]
                in  (p,zipWith (-) (tail gs) gs)

work :: Int -> [(Int,[Int])] -> Int
work n ((p,ds):pds)
    | p2 > n    = n
    | otherwise = let ms = takeWhile (0 <) $ iterate (`div` p) (n `div` p2)
                  in  (+) (work n pds) $! sum (zipWith (\m d -> d * work m pds) ms ds)
    where 
    p2 = p*p
