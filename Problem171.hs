module Problem171 where

import qualified Data.MemoCombinators as Memo

numContinuations :: Int -> Int -> Integer
numContinuations = Memo.memo2 (Memo.arrayRange (0,19)) (Memo.arrayRange (0,1620)) numContinuations'
    where numContinuations' 0   f_n
            | f_n == r*r            = 1
            | otherwise             = 0
            where r = floor . sqrt $ fromIntegral f_n
          numContinuations' len f_n = sum [ numContinuations (len-1) (f_n+d^2) | d <- [0..9] ]

solution :: Int -> Int -> Integer
solution len dig = ((10^dig-1) `div` 9 * pos) `mod` (10^dig)
    where pos = sum [ numContinuations (len-1) (d^2) * fromIntegral d | d <- [0..9] ]

main = print $ solution 20 9

{-isSquare :: Integral a => a -> Bool
isSquare n = n == r*r
    where r = floor . sqrt $ fromIntegral n

solution_spec :: Int -> Int -> Integer
solution_spec len dig = sum [ n | n <- [1..10^len-1], isSquare . sum . map (^2) $ digits n ] `mod` (10^dig)-}