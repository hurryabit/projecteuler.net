module Problem44 where

import Control.Monad
import Data.Array.IArray
import Data.List

import qualified Numbers

pentagonals :: [Integer]
pentagonals = scanl1 (+) [1,4..]

isPentagonal :: Integer -> Bool
isPentagonal a =
  let m' :: Double
      m' = (1+sqrt (1+24*fromIntegral a))/6
      m  = round m'
  in  (3*m-1)*m == 2*a

pentagonal :: Integer -> Integer
pentagonal n = n*(3*n-1) `div` 2

divisorsArray = Numbers.divisorsArray 30000

sol1 m = do
  let m3 = 3*m-1
  p1 <- divisorsArray ! m
  p2 <- divisorsArray ! m3
  let p = p1*p2
  guard $ p > m3 && p `mod` 3 == 2
  let k = (m*m3) `div` p
      (n,r) = (p+1+3*k) `divMod` 6
  guard $ r== 0
  return (n,k)

sol m = let s1 = sol1 m 
            s2 = [ (n-k,k) | (n,k) <- s1 ]
        in  [ (n,k,l) | (n,k) <- s1, (n',l) <- s2, n == n' ]
