import Data.MemoCombinators (Memo)
import Numeric.Search.Range (searchFromTo)

import qualified Data.MemoCombinators as Memo

main = print solution

solution :: Int
solution = 4 * sum [ numTriangles (x,y) | x <- xrange, y <- [0 .. ymax x] ] `div` 3

radius :: Int
radius = 500

isqrt :: Int -> Int
isqrt n = maybe n pred (searchFromTo (\k -> k*k > n) 0 n)

ymax :: Int -> Int
ymax = Memo.arrayRange (1,radius-1) $ \x -> isqrt (radius^2 - x^2 - 1)

type Point = (Int,Int)

area :: Int
area = sum (map ((1+) . ymax) xrange)

areaBelow, areaAbove, numColinear :: Point -> Int
areaBelow = memoPoint $ \(x0,y0) -> if y0 == 0 then 0 else areaAbove (y0,x0) + (radius-1)
areaAbove = memoPoint $ \(x0,y0) -> sum $ takeWhile (0 <) [ ymax x - (y0*x `div` x0) | x <- xrange ]
numColinear p = area - (areaBelow p + areaAbove p)

accumArea :: Int
accumArea = sum [ accumAreaBelow (x,ymax x) | x <- xrange ]

accumAreaBelow, accumAreaAbove, accumNumColinear :: Point -> Int
accumAreaBelow   = memoPoint $ \(x,y) -> areaBelow   (x,y) + if y == 0      then 0 else accumAreaBelow   (x,y-1)
accumAreaAbove   = memoPoint $ \(x,y) -> areaAbove   (x,y) + if y == ymax x then 0 else accumAreaAbove   (x,y+1)
accumNumColinear = memoPoint $ \(x,y) -> numColinear (x,y) + if y == ymax x then 0 else accumNumColinear (x,y+1)

numTriangles :: Point -> Int
numTriangles p0 = numTypeA p0 + numTypeB p0 + numTypeC p0

numTypeA, numTypeB, numTypeC :: Point -> Int
numTypeA p0@(x0,y0) = areaAbove p0 * areaAbove p0 - sum [ accumAreaAbove p + accumNumColinear p | p <- ps ]
  where
    ps = takeWhile (\(x,y) -> y <= ymax x) [ (x,y0*x `div` x0+1) | x <- xrange ]
numTypeB p0         = area * areaAbove p0 + accumArea
numTypeC p0@(x0,y0)
  | y0 == 0         = 0
  | otherwise       = areaBelow p0 * (areaAbove p0 + area) + sum (map accumAreaBelow ps)
  where
    ps = [ (x,min (ymax x) ((y0*x-1) `div` x0)) | x <- xrange ]

xrange :: [Int]
xrange = [1 .. radius-1]

memoX, memoY :: Memo Int
memoX = Memo.unsafeArrayRange (1,radius-1)
memoY = Memo.unsafeArrayRange (0,radius-1)

memoPoint :: Memo Point
memoPoint = Memo.pair memoX memoY
