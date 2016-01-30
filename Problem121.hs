module Problem121 where

solution = floor $ product [2..16] / fromIntegral (nominator 15 7)

nominator :: Integer -> Int -> Integer
nominator _ 0 = 1
nominator 0 _ = 1
nominator n k = n*nominator (n-1) (k-1) + nominator (n-1) k
