module Problem75 where

import Control.Monad
import Data.Array.IArray
import Numbers

divis :: Array Int [Int]
divis = divisorsArray 1000000

solutions :: Int -> [(Int,Int)] -- (n,k)
solutions q = do
  n <- divis ! q
  guard $ odd n
  k <- divis ! (q `div` n)
  guard $ k < n && n < 2*k && gcd n k == 1
  return (n,2*k-n)

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

sol = length $ filter (isSingleton . solutions) [1..1000000]

main = print sol
