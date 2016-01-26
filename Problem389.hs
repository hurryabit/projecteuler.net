{-# LANGUAGE BangPatterns #-}
module Problem389 where

import Control.Arrow hiding ((<+>))
import Data.Array.Unboxed
import Data.List
import Data.Ratio

-- SOLUTION --

type Distr = [Double]

zipAdd' :: Num a => [a] -> [a] -> [a]
zipAdd' []     ys     = ys
zipAdd' xs     []     = xs
zipAdd' (x:xs) (y:ys) = let !z = x+y in z:zipAdd' xs ys 

distrDice :: Int -> [Distr]
distrDice t = iterate (map (/ fromIntegral t) . foldl1' zipAdd' . take t . drop 1 . iterate (0:)) [1]

distrD = foldl f [0,1] [4,6,8,12]
    where
    f ps t = foldl1' zipAdd' $ zipWith (map . (*)) ps (distrDice t)

solve = let (e,esq) = foldl' f (0,0) $ zip distrD [0..]
        in  esq - e*e
    where
    f (e,esq) (p,n) = (e + p*fromIntegral n*e20,esq + p*(fromIntegral n*esq20+fromIntegral (n*(n-1))*e20*e20))
    e20 = 10.5
    esq20 = 143.5

main = print solve

-- EXPERIMENTS --

type Prob = Double

type Var a = Array a Double

(<+>) :: Var Int -> Var Int -> Var Int
x <+> y = let rx@(lx,hx) = bounds x
              ry@(ly,hy) = bounds y
          in  accumArray (+) 0 (lx+ly,hx+hy) [ (vx+vy,px*py) | (vx,px) <- assocs x, (vy,py) <- assocs y ]

die :: Int -> Var Int
die t = listArray (1,t) (repeat (1 / fromIntegral t))

dice :: Int -> Int -> Var Int
dice n t = run n (die t) (cnst 0)
    where run 0 x y = y
          run k x y = let (q,r) = k `divMod` 2
                      in  run q (x <+> x) (if r == 0 then y else y <+> x)

cnst :: Int -> Var Int
cnst v = listArray (v,v) [1]

bind :: Var Int -> (Int -> Var Int) -> Var Int
bind x yf = let (ls,hs,vpss) = unzip3 [ (l,h,map (second (px*)) (assocs y))| (vx,px) <- assocs x, let y = yf vx, let (l,h) = bounds y ]
            in  accumArray (+) 0 (minimum ls,maximum hs) (concat vpss)

mapMonotonic :: (Int -> Int) -> Var Int -> Var Int
mapMonotonic f x = accumArray (+) 0 ((f *** f) (bounds x)) $ map (first f) (assocs x)

expectMap :: (Int -> Int) -> Var Int -> Prob
expectMap f = foldl' (\acc (v,p) -> acc + p * fromIntegral (f v)) 0 . assocs

expect :: Var Int -> Prob
expect = expectMap id

variance :: Var Int -> Prob
variance x = let (ex1,ex2) = foldl' (\(acc1,acc2) (v,p) -> (acc1 + p * fromIntegral v,acc2 + p * fromIntegral (v*v))) (0,0) $ assocs x
             in  ex2 - ex1^2

level1 = die 4
level2 = level1 `bind` flip dice 6
level3 = level2 `bind` flip dice 8
level4 = level3 `bind` flip dice 12