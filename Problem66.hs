module Problem66 where

import Control.Monad
import Data.List
import Data.Ord
import Data.Ratio

evalRep :: [Integer] -> Rational
evalRep = foldl1 (\f d -> d+1/f) . map fromIntegral

fractions :: Integer -> [Rational]
fractions = map evalRep . tail . reverseInits . representation

representation :: Integer -> [Integer]
representation n = a0:cycle (run c)
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

reverseInits :: [a] -> [[a]]
reverseInits = scanl (flip (:)) []

isSquare :: Integer -> Bool
isSquare n = let r = floor (sqrt (fromIntegral n)) in r*r==n

minimalSolution :: Integer -> (Integer,Integer)
minimalSolution d = head $ do
  f <- fractions d
  let x = numerator f
      y = denominator f
  guard $ x*x-d*y*y == 1
  return (x,y)

result = maximumBy (comparing (fst . snd))
  [ (d,minimalSolution d) | d <- [1..1000], not (isSquare d) ]
