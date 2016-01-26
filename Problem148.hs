module Problem148 where

import Data.Function
import Data.List

pascal7 :: [Integer]
pascal7 = map (genericLength . filter ((0 /=) . (`mod` 7)) . takeWhile (>0)) . fix $ (:) (1:repeat 0) . map (\l -> zipWith (+) (0:l) l)

pascal7sum :: [Integer]
pascal7sum = scanl (+) 0 pascal7

p7s :: Integer -> Integer
p7s = genericIndex pascal7sum

every7 (x:xs) = x:every7 (drop 6 xs)

solution :: Integer -> Integer
solution n = let d7 = toBase 7 n
             in  fromBase 28 $ zipWith (*) (map (\r -> (r*(r+1)) `div` 2) d7) (scanl (*) 1 $ map succ d7)

toBase :: Integer -> Integer -> [Integer]
toBase b = run []
    where run ds 0 = ds
          run ds n = let (m,d) = n `divMod` b
                     in  run (d:ds) m

fromBase :: Integer -> [Integer] -> Integer
fromBase b = foldl' (\r d -> b*r+d) 0
