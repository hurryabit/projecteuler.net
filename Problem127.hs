module Main where

import Control.Monad
import Data.Array.IArray
import qualified Data.IntSet as Set
import Numbers

radicalArray :: Array Int Int
radicalArray = amap (product . map fst) $ factorizationArray 110000

rad :: Integral a => Int -> a
rad n = fromIntegral $ radicalArray ! n

candidates :: Int -> [(Int,Int)]
candidates m = do
  a <- [1..m `div` 2]
  let rad_a = rad a :: Integer
      max_b = m - a
  b <- dropWhile (<=a) . takeWhile (<max_b) $ bs
  let rad_b = rad b :: Integer
      rad_ab = rad_a * rad_b
  guard $ rad_ab < fromIntegral b && gcd rad_a rad_b == 1
  let c = a+b
      rad_c = rad c :: Integer
  guard $ rad_a*rad_b*rad_c < fromIntegral c
  return (a,b)
  where bs = [ b | b <- [2..m-1], rad b < b `div` 2]

number :: Int -> Int
number = sum . map (uncurry (+)) . candidates

good a b = gcd a b == 1 && rad a*rad b*rad c < (fromIntegral c :: Integer)
  where c = a+b

sols = candidates 110000

main = writeFile "problem127.txt" . show $ sols
