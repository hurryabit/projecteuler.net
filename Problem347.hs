{-# LANGUAGE TupleSections #-}
module Problem347 where

import Control.Monad
import Numbers

limit :: Int
limit = 10000000

bases :: [(Int,Int)]
bases = run primes
    where run :: [Int] -> [(Int,Int)]
          run (p:ps)
              | null qs   = []
              | otherwise = map (p,) qs ++ run ps
              where qs = takeWhile (<= (limit `div` p)) ps

log' :: Int -> Int -> Int
log' b n = floor $ logBase (fromIntegral b) (fromIntegral n)

em :: Int -> Int -> Int
em p q = maximum $ do
    k <- [1..log' p limit]
    l <- [1..log' q limit]
    let n = p^k * q^l
    guard $ n <= limit
    return n

main = print . sum . map (uncurry em) $ bases