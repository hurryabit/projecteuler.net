module Problem89 where

import Data.Char
import Data.List.Utils

main = readFile "roman.txt" >>= print . sum . map savings . split "\r\n"

savings :: String -> Int
savings r = let n = readRoman r
            in  length r - length (showRoman n)

romanDigit :: Char -> Int
romanDigit c = case toUpper c of
  'I' ->    1
  'V' ->    5
  'X' ->   10
  'L' ->   50
  'C' ->  100
  'D' ->  500
  'M' -> 1000

readRoman :: String -> Int
readRoman ""  = 0
readRoman [c] = romanDigit c
readRoman (c1:c2:cs)
  | n1 < n2   = n2 - n1 + readRoman cs
  | otherwise = n1 + readRoman (c2:cs)
  where n1 = romanDigit c1
        n2 = romanDigit c2

showRoman :: Int -> String
showRoman n = replicate d1000 'M' ++ showDigit d100 ('C','D','M') ++
  showDigit d10 ('X','L','C') ++ showDigit d1 ('I','V','X')
  where (r1,d1) = n `divMod` 10
        (r10,d10) = r1 `divMod` 10
        (d1000,d100) = r10 `divMod` 10
        showDigit k (a,b,c)
          | k <  4 = replicate k a
          | k == 4 = [a,b]
          | k <  9 = b:replicate (k-5) a
          | k == 9 = [a,c]
