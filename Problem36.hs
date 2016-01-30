module Problem36 where

import Data.Char

isPalindromicBy :: (a -> String) -> a -> Bool
isPalindromicBy shower n = let s = shower n in s == reverse s

isPalindromic10 :: Int -> Bool
isPalindromic10 = isPalindromicBy show

isPalindromic2 :: Int -> Bool
isPalindromic2 = isPalindromicBy show2

isPalindromic10_2 :: Int -> Bool
isPalindromic10_2 n = isPalindromic10 n && isPalindromic2 n

show2 :: Int -> String
show2 0 = ""
show2 n = let (q,r) = n `divMod` 2 in intToDigit r:show2 q
