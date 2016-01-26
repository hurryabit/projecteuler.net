module Problem59 where

import Data.Bits
import Data.Char
import Data.List
import System.IO.Unsafe

cipher :: [Int]
cipher =  let contents = unsafePerformIO . readFile $ "cipher1.txt"
          in  read $ "[" ++ contents ++ "]"

keys :: [[Int]]
keys = sequence (replicate 3 [fromEnum 'a' .. fromEnum 'z'])

decrypt :: [Int] -> [Int] -> String
decrypt cph key = map toEnum $ zipWith xor cph (cycle key)

plains :: [String]
plains = filter isText . map (decrypt cipher) $ keys

isText :: String -> Bool
isText txt = countBelow (\c -> not (isAlphaNum c || isSpace c)) 50 txt

countBelow :: (a -> Bool) -> Int -> [a] -> Bool
countBelow p n = null . drop n . filter p

sol = sum . map fromEnum . head $ plains
