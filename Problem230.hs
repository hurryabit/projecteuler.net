module Problem230 where

import Data.Array
import Data.Function
import Data.List

word1 = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

word2 = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"

fibs :: Array Int Integer
fibs = listArray (1,100) $
  fix $ \fs -> genericLength word1:genericLength word2:zipWith (+) fs (tail fs)

-- digit n k => k-te Stelle im n-ten Wort
digit :: Int -> Integer -> Char
digit 1 k = word1 `genericIndex` (k-1)
digit 2 k = word2 `genericIndex` (k-1)
digit n k
  | k <= b    = digit (n-2) k
  | otherwise = digit (n-1) (k-b)
  where b = fibs ! (n-2)

d :: Integer -> Char
d k = let Just (n,_) = find ((k <=) . snd) (assocs fibs)
      in  digit n k

sol = [ d ((127+19*n)*7^n) | n <- [17,16..0] ]
