module Problem448 where

import Numbers

specAvg :: Integer -> Integer
specAvg n = sum [ lcm n i | i <- [1..n] ] `div` n

{-implAvg :: Integer -> Integer
implAvg n = foldl -}

sum' = sum . enumFromTo 1

avg_p p = avg_p2e p 1

avg_pq p q = --sum [1..p*q] - (p-1)*avg_p q - (q-1)*avg_p p - (p-1)*(q-1)
    sum [1..p*q] - sum' p*sum' q + avg_p p*avg_p q

avg_psq p q = --sum' (p^2*q) - sum' (p*q)*(p-1) - sum' q*(p-1) - sum' (p^2)*(q-1) + sum' p*(p-1)*(q-1) + (p-1)*(q-1)
    sum' (p^2*q) - sum' (p*q)*(p-1) - sum' q*(p-1) - avg_p2e p 2*(q-1)

avg_p2e p 0 = 1
avg_p2e p e = sum [1..p^e] - p*sum [1..p^(e-1)] + avg_p2e p (e-1)

{-sumAvg :: Integer -> Integer
sumAvg m = sum [ avg n | n <- [1..m] ]

lcmPattern :: Integer -> [Integer]
lcmPattern = foldl1 (zipWith (*)) . map (cycle . uncurry ppPattern). factorization
    where ppPattern :: Integer -> Int -> [Integer]
          ppPattern p 1 = 1:replicate (fromIntegral p-1) p
          ppPattern p e = let qs = map (p*) $ ppPattern p (e-1)
                          in  1:tail qs ++ concat (replicate (fromIntegral p-1) qs)

sumAvg' m = m + sum [ sum $ take (fromIntegral $ m-i+1) (lcmPattern i) | i <- [2..m] ]-}