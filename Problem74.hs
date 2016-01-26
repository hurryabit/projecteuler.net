module Main where

import Data.Array.Unboxed
import Data.List

facs :: UArray Int Int
facs = array (0,9) [ (n,product [1..n]) | n <- [0..9] ]

next :: Int -> Int
next = sum . map (facs !) . digits

digits :: Int -> [Int]
digits = unfoldr f
  where f 0 = Nothing
        f n = let (q,r) = n `divMod` 10 in Just (r,q)

chains :: Array Int [Int]
chains =
  listArray size [ takeWhileUnique $ n:(chains ! next n) | n <- range size ]
  where size = (1,2177281)

chain :: Int -> [Int]
chain = takeWhileUnique . iterate next

sol :: Int
sol = length . filter ((60==) . length . (chains !)) $ [1..1000000]

main = print sol

takeWhileUnique :: Eq a => [a] -> [a]
takeWhileUnique = takeWhileAccum (\xs x -> (x:xs,x `notElem` xs)) []

takeWhileAccum :: (acc -> a -> (acc,Bool)) -> acc -> [a] -> [a]
takeWhileAccum _ _ [] = []
takeWhileAccum p a (x:xs) =
  let (a',b) = p a x
  in  if b then x:takeWhileAccum p a' xs else []
