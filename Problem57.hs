module Problem57 where

import Data.Ratio

expansions :: [Rational]
expansions = take 1000 . tail . iterate (\n -> 1 + 1 / (1+n)) $ 1

isSolution :: Rational -> Bool
isSolution r = f (numerator r) > f (denominator r)
  where f = length . show
