module Problem486 where

import Data.Bits
import Data.List
import Data.Numbers.Primes

numbers :: Int -> [(Int,Int,Int)]
numbers m = zip3 [6..] (iterate (\x -> (2*x+1) `mod` m) 127) (scanl (\x y -> (x+y) `mod` m) 85 $ cycle [32,32,32,34,36,34])

positions :: Int -> [Int]
positions m = [ k | (k,x,y) <- take (3*m*(m-1)) (numbers m), x == y ]

main = print $ length $ positions 4877

sameBit n i j = testBit n i == testBit n j

pal :: Bool -> Int -> Bool
pal b n = (sameBit n 0 4 && sameBit n 1 3) || (b && sameBit n 0 5 && sameBit n 1 4 && sameBit n 2 3)

graph = do
    np <- filter (not . pal False) [0x00 .. 0x1F]
    np' <- map (0x1F .&.) $ filter (not . pal True) [np `shiftL` 1, np `shiftL` 1 .|. 0x01]
    return (np,np')

level 0 = filter (not . pal False) [0x00 .. 0x1F]
level k = map snd $ filter ((`elem` level (k-1)) . fst) graph

-- SPEC

containsPalindrom :: [Int] -> Bool
containsPalindrom = any p . transpose . take 6 . iterate (drop 1)
    where
    p (a:b:c:d:e:fs) = (a == e && b == d) || ([a] == fs && b == e && c == d)
    p _              = False

strings :: Int -> [[Int]]
strings n = sequence (replicate n [0,1])