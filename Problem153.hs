module Problem153 where

import qualified Data.MemoCombinators as Memo
import ListPlus
import Sieve

sigmaSum_spec :: Int -> Int
sigmaSum_spec n = sum . elems $ sieveTable (sigmaSC 1) n

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

thresh :: Int
thresh = 10^4

sigmaSumTable :: Array Int Int
sigmaSumTable = listArray (1,thresh) . scanl1 (+) . elems $ sieveTable (sigmaSC 1) thresh

sigmaSum :: Int -> Int
sigmaSum n
    | n <= thresh = sigmaSumTable ! n
    | otherwise   = let r = intSqrt n
                        bs = map (div n) [2..r]
                        clus = zip3 [1..] (map succ bs) (n:bs)
                        u0 = n `div` r
                        s1 = sum [ n `div` d * d | d <- [1..u0] ]
                        s2 = sum [ (l+u)*(u-l+1) `div` 2 * c | (c,l,u) <- clus ]
                    in  s1+s2

gaussSigmaSum :: Int -> Int
gaussSigmaSum n = sigmaSum n + sum [ 2 * a * sigmaSum (n `div` (a*a+b*b)) | a <- [1..intSqrt n], b <- [1..intSqrt (n-a^2)], gcd a b == 1 ]

main = print $ gaussSigmaSum $ 10^8

-- 17971254122360635