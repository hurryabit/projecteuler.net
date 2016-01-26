module Problem205 where

import Control.Applicative
import qualified Data.IntMap as Map
import Data.Ratio

type Distribution = [(Int,Rational)]

zero :: Distribution
zero = [(0,1)]

times :: Distribution -> Distribution -> Distribution
times d1 d2 = Map.assocs . Map.fromListWith (+) $
  (\(m,p) (n,q) -> (m+n,p*q)) <$> d1 <*> d2

dice :: Int -> Distribution
dice n = [ (i,1%fromIntegral n) | i <- [1..n] ]

pete, colin :: Distribution
pete = foldl times zero (replicate 9 (dice 4))
colin = foldl times zero (replicate 6 (dice 6))

sol = sum [ p*q | (m,p) <- pete, (n,q) <- colin, m > n ]
