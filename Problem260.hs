module Problem260 where

import Data.List

type Position = [Int]

data Type = Type Int Int Int

instance Show Type where
  show (Type d1 d2 b) = show (d1,d2) ++ " -> " ++ show b

types = map (\[x,y,z] -> Type (y-x) (z-y) x) losePositions

losePositions :: [Position]
losePositions = sieve positions
    where sieve (p:ps) = p:sieve (filter (not . flip successorOf p) ps)

positions :: [Position]
positions = [ [x,y,s-x-y] | s <- [0..], x <- [0..s `div` 3], y <- [x..(s-x) `div` 2] ]

successorOf :: Position -> Position -> Bool
successorOf pos = any good . permutations
  where good pos2 = case sort $ zipWith (-) pos pos2 of
                      [0,0,_] -> True
                      [0,y,z] -> y==z
                      [x,y,z] -> x > 0 && x == y && y == z

counts = pad 0 . map (\xs -> (head xs,length xs)) . group . map sum $ losePositions

pad z ((x,y):xys)
    | z == x = y:pad (z+1) xys
    | z < x  = 0:pad (z+1) ((x,y):xys)

{-import Data.List hiding (delete)

delete :: Ord a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys) = case x `compare` y of
  LT -> y:ys
  EQ -> ys
  GT -> y:delete x ys

generate :: Int -> [Int] -> [(Int,Int)]
generate m (n:ns) = (n,k):generate (m+1) (delete k ns)
  where k = m+n

pairSeq :: [(Int,Int)]
pairSeq = generate 0 [0..]

diffSeq :: [Int]
diffSeq = runLength . runLength . map (\n -> n-1) $ zipWith (-) (tail ps) ps
  where ps = map fst pairSeq
        runLength = map length . group-}
