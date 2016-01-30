module Problem64 where

import Data.Ratio

solution = length . filter (even . length . fraction) . filter (not . isSquare) $ [1..10000]

isSquare :: Integer -> Bool
isSquare n =  let k = floor . sqrt . fromIntegral $ n
              in  n==k*k


fraction :: Integer -> [Integer]
fraction n = a0:run c
  where (a0,c) = evolve n (0,1)
        run d = let (ak,d') = evolve n d
                in  ak:(if d' == c then [] else run d')

evolve :: Integer -> (Rational,Rational) -> (Integer,(Rational,Rational))
evolve n (a,b) =  let n' = fromIntegral n
                      x :: Double
                      x = fromRational a + fromRational b * sqrt (fromIntegral n)
                      k :: Integer
                      k = floor x
                      k' = fromIntegral k
                      d = (b^2*n'-(k'-a)^2)
                  in  (k,((k'-a)/d,b/d))
