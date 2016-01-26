module Problem101 where

import Control.Arrow
import Data.List
import Data.Ratio

u :: Rational -> Rational
u n = sum [ (-n)^i | i <- [0..10] ]

q :: Rational -> Rational
q n = n^3

fit :: (Rational -> Rational) -> (Rational -> Rational) -> Rational
fit f g = let Just (_,y) = find (uncurry (/=)) [ (f x,g x) | x <- [1..] ]
          in  y

interpolate :: [(Rational,Rational)] -> (Rational -> Rational)
interpolate xyis x = sum [ yi * l xi | (xi,yi) <- xyis ]
  where xjs = map fst xyis
        l xi = product [ (x - xj)/(xi-xj) | xj <- xjs, xi /= xj ]

interpolation :: Int -> (Rational -> Rational) -> (Rational -> Rational)
interpolation n f x = interpolate [ (i,f i) | i <- [1..fromIntegral n] ] x
