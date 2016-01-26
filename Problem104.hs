module Main where

import Data.List

import Numbers

isPandigital :: [Integer] -> Bool
isPandigital ds = sort ds == [1..9]

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

fibsMod :: [Integer]
fibsMod = 0:1:zipWith (\x y -> (x+y) `mod` 1000000) fibsMod (tail fibsMod)

fibsN :: [(Integer,Integer)]
fibsN = zip [0..] fibs

isL9P :: Integer -> Bool
isL9P = ([1..9]==) . sort . take 9 . digits

isF9P :: Integer -> Bool
isF9P = (['1'..'9']==) . sort . take 9 . show

fibsL9P :: [(Integer,Integer)]
fibsL9P = filter (isL9P . snd) fibsN

fibsF9P :: [(Integer,Integer)]
fibsF9P = filter (isF9P . snd) fibsN

fibsFL9P :: [(Integer,Integer)]
fibsFL9P = filter (isF9P . snd) fibsL9P

main = print . fst . head $ fibsFL9P
