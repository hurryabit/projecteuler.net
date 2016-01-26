module Problem80 where

import Data.List

sqrtDigits :: Integer -> [Integer]
sqrtDigits = unfoldr (Just . f) . (,) 0
  where f (k,n) = (d,(k',100*n))
          where (d,k')= head [ (e,l) | e <- [9,8..0], let l=10*k+e, l*l<=n ]

sol = (sum . map (sum . take 100 . sqrtDigits) $ [1..99]) - 45
