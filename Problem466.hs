{-# LANGUAGE BangPatterns #-}
module Problem466 where

import Prelude hiding (cos,sin)
import Data.List
import qualified Data.Map.Strict as Map

neg :: (Int,Int) -> (Int,Int)
neg (!p,!q) = (-p,-q)

(<+>) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(!p1,!q1) <+> (!p2,!q2) = (p1+p2,q1+q2)

cos :: Int -> (Int,Int)
cos a = case a of
    36  -> ( 0, 2)
    108 -> ( 1,-2)
    180 -> (-2, 0)
    252 -> ( 1,-2)
    324 -> ( 0, 2)
    _ -> error $ "cos " ++ show a

sin :: Int -> (Int,Int)
sin a = case a of
    36  -> ( 1, 0)
    108 -> ( 0, 1)
    180 -> ( 0, 0)
    252 -> ( 0,-1)
    324 -> (-1, 0)


data Position = Position
    { posX :: (Int,Int)
    , posY :: (Int,Int)
    , posA :: Int
    }
    deriving (Show, Eq, Ord)

data Direction = L | R
    deriving (Show, Eq)

advance1 :: Map.Map Position Int -> Map.Map Position Int
advance1 = Map.fromListWith (+) . concatMap (\(p,k) -> [(execute1 L p,k),(execute1 R p,k)]) . Map.toList

main = print $ Map.lookup origin $ foldl1' (.) (replicate 70 advance1) $ Map.singleton origin 1

pathsLen :: Int -> [[Direction]]
pathsLen 0 = [[]]
pathsLen n = let ps = pathsLen (n-1)
             in  map (L:) ps ++ map (R:) ps

paths :: [[Direction]]
paths = concatMap pathsLen [0..]

circles :: [[Direction]]
circles = filter (\ds -> isOrigin (execute ds origin )) paths

execute1 :: Direction -> Position -> Position
execute1 d (Position !x !y !a) =
    let da = case d of
                L -> -36
                R ->  36
        a' = (a+da) `mod` 360
    in  Position (x <+> cos a') (y <+> sin a') ((a'+da) `mod` 360)

execute :: [Direction] -> Position -> Position
execute ds p = foldl (flip execute1) p ds

origin :: Position
origin = Position (0,0) (0,0) 0

isOrigin :: Position -> Bool
isOrigin (Position (0,0) (0,0) _) = True
isOrigin _                        = False

simplify :: [Direction] -> [Direction]
simplify = whileJust simplify1

simplify1 :: [Direction] -> Maybe [Direction]
simplify1 ds = case break ((5 <=) . length) (group ds) of
    (ls,r:rs) -> Just $ concat $ ls ++ (take (length r `mod` 5) r):rs
    _         -> Nothing

whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = maybe x (whileJust f) (f x)
