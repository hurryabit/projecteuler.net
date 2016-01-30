module Problem94 where

import Data.List

seq1, seq2 :: [Integer]
seq1 = 6:66:zipWith (\x0 x1 -> 14*x1-x0-16) seq1 (tail seq1)
seq2 = 16:240:zipWith (\x0 x1 -> 14*x1-x0+16) seq2 (tail seq2)

sol1 = takeWhile (<= 1000000000) . map ((+(-2)) . (*3)) $ seq1
sol2 = takeWhile (<= 1000000000) . map ((+2) . (*3)) $ seq2

solution1 = filter isGood1 [2,4..333333332]
solution2 = filter isGood2 [2,4..333333332]

isGood1 :: Integer -> Bool
isGood1 a = (truncate $ sqrt $ fromIntegral d)^2 == d
  where d = 3*a*a+8*a+4

isGood2 :: Integer -> Bool
isGood2 a = (truncate $ sqrt $ fromIntegral d)^2 == d
  where d = 3*a*a-8*a+4
