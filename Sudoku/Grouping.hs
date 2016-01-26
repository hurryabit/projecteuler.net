module Sudoku.Grouping
  ( Index
  , Group
  , Grouping
  , groups
  , relation
  , stdGrouping
  ) where

import Data.Ix (range)

type Index = (Int,Int)

type Group = [Index]

type GRelation = Index -> Index -> Bool

data Grouping = Grouping
  { groups    :: [Group]
  , relation  :: GRelation
  }

rowGroups :: Int -> [Group]
rowGroups n = [ [ (r,c) | c <- indices ] | r <- indices ]
  where indices = [0..n-1]

colGroups :: Int -> [Group]
colGroups n = [ [ (r,c) | r <- indices ] | c <- indices ]
  where indices = [0..n-1]

squareGroups :: Int -> [Group]
squareGroups n = [ [ (n*p+r,n*q+c) | (r,c) <- indices ] | (p,q) <- indices ]
  where indices = range ((0,0),(n-1,n-1))

stdGrouping :: Int -> Grouping
stdGrouping n = Grouping
  { groups    = rowGroups (n*n) ++ colGroups (n*n) ++ squareGroups n
  , relation  = stdRelation n
  }

stdRelation :: Int -> GRelation
stdRelation n (r1,c1) (r2,c2) = (r1 /= r2 || c1 /= c2) &&
    (r1 == r2 || c1 == c2 ||
    (r1 `div` n == r2 `div` n && c1 `div` n == c2 `div` n))
