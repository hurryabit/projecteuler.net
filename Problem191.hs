module Problem191 where

import Data.Array

strings n = sequence (replicate n "OAL")

isPriceString = isPS 0 False
  where isPS _ _ [] = True
        isPS a l ('O':xs) = isPS 0 l xs
        isPS a l ('A':xs) = a < 2 && isPS (a+1) l xs
        isPS a l ('L':xs) = not l && isPS 0 True xs

priceStrings = filter isPriceString . strings

table :: Array (Int,Bool) Integer
table = listArray size $ map (uncurry f') (range size)
  where size = ((1,False),(81,True))
        f' :: Int -> Bool -> Integer
        f' n l
          | n <= 3 =  let ps = if l then "OAL" else "OA"
                      in  fromIntegral . length . filter isPriceString .
                            sequence . replicate n $ ps
        f' n True  = sum [ f (n-k) l | k <- [1..3], l <- [True,False] ]
        f' n False = sum [ f (n-k) False | k <- [1..3] ]

f :: Int -> Bool -> Integer
f n l = table ! (n,l)

