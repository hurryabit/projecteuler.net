module Main where

import Data.Array
import Data.List

import Primes

main = print $ length . filter isSolution $ primes

rotations :: [a] -> [[a]]
rotations xs = zipWith (++) (tails xs) (inits xs)

isSolution :: Int -> Bool
isSolution = all isPrime . map read . rotations . show
