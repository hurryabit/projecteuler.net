import Data.Array
import Data.List

import Numbers

dT :: Array Int Int
dT = listArray (1,bnd) $ zipWith3 f [1.. ] numDivs (tail numDivs)
  where
    bnd = 60000000
    numDivs = elems $ numDivisorsArray (bnd+1)
    f n d1 d2
      | even n    = g n     d1 d2
      | otherwise = g (n+1) d2 d1
    g m d1 d2 =
      let (_,k) = divMaxPow m 2
      in  (d1 `div` (k+1)) * k * d2

main = print $ maximum $ elems dT