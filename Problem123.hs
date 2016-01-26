module Problem123 where

import Numbers

sol = take 2 . dropWhile (\(p,n) -> 2*p*n < 10000000000) $ zip primes [1..]
