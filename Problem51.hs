module Problem51 where

import Data.Array

import qualified Numbers as N

isPrimeArray :: Array Int Bool
isPrimeArray = N.isPrimeArray 999999

isPrime :: Int -> Bool
isPrime p = isPrimeArray ! p
--isPrime p
--  | inRange (bounds isPrimeArray) p = isPrimeArray ! p
--  | otherwise                       = error $ "index out of bounds " ++ show p

primes :: [Int]
primes = N.primesFrom isPrimeArray

family :: Int -> [Maybe Int] -> [Int]
family c n = [ N.foldDigits . map (maybe d id) $ n | d <- [c..9] ]

isSolution :: Int -> Bool
isSolution n =
  any (\d -> any (atMost (2-d) (not . isPrime) . family d) (patterns d n)) [0..2]

patterns :: Int -> Int -> [[Maybe Int]]
patterns d = init . run . N.digits
  where run [] = [[]]
        run (x:xs)
          | x == d    = map (Nothing:) xss ++ yss
          | otherwise = yss
          where yss = map ((Just x):) xss
                xss = run xs

atMost :: Int -> (a -> Bool) -> [a] -> Bool
atMost n p = null . drop n . filter p

main :: IO ()
main = mapM_ print . filter isSolution . dropWhile (<10) $ primes
