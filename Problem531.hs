module Problem531 where

import Control.Monad
import Data.Array

import Numbers hiding (phi)

factorizations :: Array Int [(Int,Int)]
factorizations = factorizationArray 1005000

lcmSplit :: Int -> Int -> (Int,Int)
lcmSplit m n = align 1 (factorizations ! m) 1 (factorizations ! n)
    where
        eval :: [(Int,Int)] -> Int
        eval = product . map (uncurry (^))
        align :: Int -> [(Int,Int)] -> Int -> [(Int,Int)] -> (Int,Int)
        align m' []               n' qls              = (m'         ,n'*eval qls)
        align m' pks              n' []               = (m'*eval pks,n'         )
        align m' pks@((p,k):pks') n' qls@((q,l):qls') = case p `compare` q of
            LT              -> align (m'*p^k) pks' n'       qls
            EQ  | k >= l    -> align (m'*p^k) pks' n'       qls'
                | otherwise -> align m'       pks' (n'*q^l) qls'
            GT              -> align m'       pks  (n'*q^l) qls'

extChinese2 :: (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
extChinese2 (a,m) (b,n) = do
    let d       = gcd m n
        (m',n') = lcmSplit m n
    guard $ a `mod` d == b `mod` d
    return $ chinese2 (a,m') (b,n')

g :: (Int,Int) -> (Int,Int) -> Int
g am bn = maybe 0 fst $ extChinese2 am bn

phi :: Int -> Int
phi = product . map (\(p,k) -> (p-1)*p^(k-1)) . (factorizations !)

main = print $ sum [ g (phi m,m) (phi n,n) | m <- [1000000 .. 1004999], n <- [m+1 .. 1004999] ]
