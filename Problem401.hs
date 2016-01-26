module Problem401 where

spec :: Integer -> Integer
spec n = sum [ sum [ d^2 | d <- [1..k], k `mod` d == 0 ] | k <- [1..n] ]

impl :: Integer -> Integer
impl n = sum [ (k-d)*d*d + (k*(2*k+1)*(k+1) - (d-1)*(2*d-1)*d) `div` 6 | d <- [1..floor (sqrt (fromIntegral n))], let k = n `div` d ]

main = print $ impl (10^15) `mod` (10^9)