{-# LANGUAGE DataKinds, FlexibleInstances, RecordWildCards, TypeOperators #-}
import Data.Array
import Data.Function
import Data.List
import Modulo

merge2D :: Ord a => [[a]] -> [a]
merge2D ([]    :xxs) =     merge2D xxs
merge2D ((x:xs):xss) = x : merge2D (insert xs xss)

type PT = (Int,Int,Int) -- (c,b,a), c^2 = b^2 + a^2, c >= b > a

primPTs :: [PT]
primPTs = merge2D [ [ ((u*u+v*v) `quot` 2, u*v, (u*u-v*v) `quot` 2) | v <- takeWhile (<u) [1,3..] ] | u <- [1,3..] ]

allPTs :: [PT]
allPTs = merge2D [ [ (k*c,k*b,k*a) | k <- [1..] ] | (c,b,a) <- primPTs ]

square :: [PT] -> [PT]
square = map (\(c,b,a) -> (c*c,b*b,a*a))

data Point = Point { x, y :: Int}
  deriving (Show, Eq)

mirrorXY :: Point -> Point
mirrorXY (Point x y) = Point y x

below :: Point -> Point -> Bool
p `below` q = (((<=) `on` x) p q) && (((<=) `on` y) p q) && p /= q

to :: Point -> Point -> Point
Point x1 y1 `to` Point x2 y2 = Point (x2-x1) (y2-y1)

inadmissiblePoints :: Int -> [Point]
inadmissiblePoints n =
  concatMap (\p -> [p,mirrorXY p]) $
    filter (`below` Point n n) $
    map (\(_,y,x) -> Point {..}) $
    takeWhile (\(z,_,_) -> z <= 2*n) $
    square allPTs

instance KnownPrime 1000000007 where

type Count = Int % 1000000007

factorialTable, recipFactorialTable :: Array Int Count
factorialTable = listArray (0,2000) $ scanl (*) 1 [1 ..]
recipFactorialTable = fmap recip factorialTable

numGridPaths :: Point -> Count
numGridPaths p = factorialTable ! (x p + y p) * recipFactorialTable ! x p * recipFactorialTable ! y p

numAdmissiblePaths :: [Point] -> Point -> Count
numAdmissiblePaths ips p = numGridPaths p - sum [ numAdmissiblePaths ips q * numGridPaths (q `to` p)| q <- takeWhile (/= p) ips, q `below` p ]