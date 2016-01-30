module Main where

import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import Data.List
import Data.Map (Map, singleton, maxViewWithKey, insertWith)
import System.IO.Unsafe

matrix :: [[Int]]
matrix = map f . lines . unsafePerformIO $ readFile "matrix.txt"
  where f xs = read $ "[" ++ xs ++ "]"

sample :: [[Int]]
sample =
  [ [131,673,234,103, 18]
  , [201, 96,342,965,150]
  , [630,803,746,422,111]
  , [537,699,497,121,956]
  , [805,732,524, 37,331]
  ]

type Index = (Int,Int)

solve :: [[Int]] -> Int
solve m = run (singleton (h,w) 0) (listArray size (repeat maxBound))
  where
    run :: Map Index Int -> UArray Index Int -> Int
    run q a = case maxViewWithKey q of
      Nothing -> a ! (1,1)
      Just ((i@(z,s),n),q')
        | a ! i <= ai'  -> run q' a
        | otherwise     -> run (insertAll q' ms) (a // [(i,ai')])
        where 
          ai' = (costs ! i) + n
          ms = [ (i,ai') | Just i <- [cond (1<z) (z-1,s),cond (z<h) (z+1,s)
                                     ,cond (1<s) (z,s-1),cond (s<w) (z,s+1)] ]
    insertAll = foldl (\q (i,n) -> insertWith min i n q)
    costs :: UArray Index Int
    costs = listArray size (concat m)
    size = ((1,1),(h,w))
    h = length m
    w = length (head m)

cond :: Bool -> a -> Maybe a
cond True  = Just
cond False = const Nothing

{-printState :: (XQueue,XArray) -> IO ()
printState (q,a) = do
  putStrLn $ replicate 80 '='
  print $ toList q
  let ((z1,s1),(z2,s2)) = bounds a
  forM_ [z1..z2] $ \z ->
    putStrLn . intercalate " " . map (\s -> showF (a ! (z,s))) $ [s1..s2]
  where
    pad s = replicate (4-length s) ' ' ++ s
    showF :: Int -> String
    showF n
      | n == maxBound = pad "***"
      | otherwise     = pad (show n)-}

main :: IO ()
main = print $ solve matrix

--demo m n = mapM_ printState . take n . drop m . solve $ sample
