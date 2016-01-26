module Problem285 where

f :: Double -> Double
f k = let alpha = asin (1 / (k+0.5))
          beta  = min (asin $ 1 / (k-0.5)) (pi/4)
      in  (pi/4-alpha)*(1+0.5/k)^2 - (pi/4-beta)*(1-0.5/k)^2 - sin (beta-alpha)*(max (1-0.5/k) (sqrt 2/k))*(1+0.5/k)