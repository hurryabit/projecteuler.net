module Problem207 where

import Data.Ratio

f :: Integer -> Rational
f n = floor (logBase 2 $ fromIntegral n) % (n-1)

solution =  let k = fst . head . dropWhile ((>=1%12345) . snd) . map (\n -> (n,f n)) $ [2..]
            in  k*(k-1)
