module Problem273 where

import Numbers (primes)

solve :: Integer -> [(Integer,Integer)]
solve n = [(a,b)  | a <- [1..truncate (sqrt (fromIntegral n/2))]
                  , let b = truncate (sqrt (fromIntegral (n-a^2))), a^2+b^2==n]

base = map (head . solve) .
  filter ((1==) . (`mod` 4)) . takeWhile (<150) $ primes

sort :: Ord a => (a,a) -> (a,a)
sort (x,y) = if x < y then (x,y) else (y,x)

combine :: (Integer,Integer) -> (Integer,Integer) -> [(Integer,Integer)]
combine (a,b) (c,d)
  | x == y    = [x]
  | otherwise = [x,y]
  where cmb (a,b) (c,d) = sort (a*c+b*d,abs (a*d-b*c))
        x = cmb (a,b) (c,d)
        y = cmb (a,b) (d,c)

pairs = foldl (\ps p -> ps ++ concatMap (combine p) ps) [(0,1)]

solution n = sum . map fst . pairs $ take n base

main = print $ solution 16
