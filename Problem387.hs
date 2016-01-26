module Problem387 where

import Control.Monad

import Numbers

rtHarshards :: [(Int,Int)]
rtHarshards = concat $ iterate step [ (n,n) | n <- [1..9] ]
    where
        step nds = do
            (n,d) <- nds
            d0 <- [0..9]
            let n' = 10*n + d0
                d' = d + d0
            guard $ d' `divides` n'
            return (n',d')

srtHarshards :: [Int]
srtHarshards = do
    (n,d) <- rtHarshards
    guard $ isPrime (n `div` d)
    return n

srthPrimes :: [Int]
srthPrimes = do
    n <- srtHarshards
    d0 <- [1,3,7,9]
    let n' = 10*n + d0
    guard $ isPrime n'
    return n'

main = print $ sum $ takeWhile (< 10^14) srthPrimes