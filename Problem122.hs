module Main where

import Data.Function
import Data.List

data Tree a = MkTree a [Tree a]
  deriving (Show)

pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith f xs = concat $ zipWith (map . f) xs (tails xs)

solutions = take 200 . nubBy ((==) `on` fst) . inOrder $ expTree

inOrder :: Tree a -> [(a,Int)]
inOrder t = inOrder' 0 [t]
  where inOrder' n ts = [ (x,n) | MkTree x _ <- ts ] ++
          inOrder' (n+1) (concat [ xs | MkTree _ xs <- ts])
  
limit :: Int -> Tree a -> Tree a
limit 0 (MkTree x _) = MkTree x []
limit n (MkTree x xs) = MkTree x $ map (limit (n-1)) xs

expTree :: Tree Int
expTree = expTree' 1 []

expTree' :: Int -> [Int] -> Tree Int
expTree' x xs =
  let ys = x:xs
      zs = filter (\z -> x<z && z <= 200) . pairWith (+) $ ys
      us = map head . group . sortBy (flip compare) $ zs
  in  MkTree x [ expTree' u ys | u <- us ]

main = print . sum . map snd $ solutions
