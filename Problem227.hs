module Problem227 where

import Prelude hiding (Rational)
import Data.Array
--import Data.Ratio

type Rational = Double

type PrX = Array Int Rational

start :: PrX
start = listArray (0,99) $ replicate 50 0 ++ 1:replicate 49 0

evolve :: PrX -> Int -> Rational
evolve prXr i = sum [ p*prXr ! ((i+j) `mod` 100) | (j,p) <- [(-2,1/36),(-1,8/36),(0,1/2),(1,8/36),(2,1/36)] ]

step :: PrX -> PrX
step prXr = listArray (0,99) $ zipWith (+) (map (evolve prXr) [0..99]) (map (*p0) corr)
    where p0 = prXr ! 0
          corr = [1/2,-8/36,-1/36] ++ replicate 95 0 ++ [-1/36,-8/36]

prY :: [Rational]
prY = 0:map (\prXr -> evolve prXr 0 - prXr ! 0/2) (iterate step start)

expY :: [Rational]
expY = scanl (+) 0 $ zipWith (*) prY [0..]

main = print . fst . head . filter (uncurry (==)) $ zip (expY') (drop 1 expY')
    where expY' = dropWhile (0 ==) expY
