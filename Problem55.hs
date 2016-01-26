module Problem55 where

import Control.Arrow

rev :: Integer -> Integer
rev = helper 0
  where helper s 0 = s
        helper s n = let (q,r) = n `divMod` 10 in helper (10*s+r) q

lychrelSeq :: Integer -> [(Integer,Integer)]
lychrelSeq n = ms 
  where ns = n:map (uncurry (+)) ms
        ms = map (id &&& rev) ns

isLychrel :: Integer -> Bool
isLychrel = not . any (uncurry (==)) . take 50 . drop 1 . lychrelSeq
