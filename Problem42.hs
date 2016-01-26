module Problem42 where

import System.IO.Unsafe

wordz :: [String]
wordz = read . unsafePerformIO $ readFile "words.txt"

value :: String -> Int
value = sum . map (\c -> fromEnum c - fromEnum 'A' + 1)

triangles :: [Int]
triangles = scanl (+) 1 [2..20]

isTriangle :: Int -> Bool
isTriangle n = elem n triangles
