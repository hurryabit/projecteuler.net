module Problem22 where

import Data.List
import System.IO.Unsafe

names :: [String]
names = read . unsafePerformIO $ readFile "names.txt"

worth :: String -> Int
worth = sum . map (\c -> fromEnum c - fromEnum 'A' + 1)

score = sum $ zipWith (\n r -> r*worth n) (sort names) [1..]
