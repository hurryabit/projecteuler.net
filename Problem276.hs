module Problem276 where

import Data.Array
import qualified Numbers as N

phiTable = N.phiArray 5000000

phi n = phiTable ! n

f :: Int -> Int -> Integer
f b c = fromIntegral $ q*phi d + length (filter ((1==) . gcd d) [l+1..l+r])
  where l = c - b
        u = min b (10000000-b-c)
        d = gcd b c
        (q,r) = (u-l) `divMod` d

g :: Int -> Integer
g c = sum [ f b c | b <- [c `div` 2 +1 .. c]]


countForPC :: Int -> Int -> Int
countForPC p c = u - l + 1
  where l = max ((p-c) `div` 2+1) (c `div` 2+1)
        u = min c (p - c - 1)
