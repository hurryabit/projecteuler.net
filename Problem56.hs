module Problem56 where

digitSum :: Integer -> Integer
digitSum = helper 0
  where helper s 0 = s
        helper s n = let (q,r) = n `divMod` 10 in helper (s+r) q

sol = maximum [ digitSum (a^b) | a <- [2..99], b <- [1..99] ]
