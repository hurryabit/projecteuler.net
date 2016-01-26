module Problem237 where

import Data.Array
import Data.MemoCombinators

data Cut = Center | Outer | Upper | Lower | OuterX | UpperX | LowerX
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

type Matrix = Array (Cut,Cut) IntX8

newtype IntX8 = IntX8 { int :: Int }
    deriving (Show,Eq,Ord)

intX8 :: Int -> IntX8
intX8 n = IntX8 $ n `mod` 100000000

instance Num IntX8 where
    IntX8 m + IntX8 n = intX8 (m+n)
    IntX8 m * IntX8 n = intX8 (m*n)
    negate (IntX8 n) = intX8 (negate n)
    abs = id
    signum (IntX8 n) = IntX8 $ signum n
    fromInteger = intX8 . fromInteger

fullBounds :: Bounded i => (i,i)
fullBounds = (minBound,maxBound)

fullRange :: (Ix i,Bounded i) => [i]
fullRange = range fullBounds

edges :: Matrix
edges = accumArray (+) 0 fullBounds $ map (\e -> (e,1))
    [ (Center,Outer )
    , (Outer ,Center)
    , (Outer ,Upper )
    , (Outer ,Lower )
    , (Outer ,OuterX)
    , (Upper ,Outer )
    , (Upper ,UpperX)
    , (Lower ,Outer )
    , (Lower ,LowerX)
    , (OuterX,Upper )
    , (OuterX,Lower )
    , (OuterX,OuterX)
    , (UpperX,Outer )
    , (UpperX,UpperX)
    , (LowerX,Outer )
    , (LowerX,LowerX)
    ]

multiply :: Matrix -> Matrix -> Matrix
multiply a b = array fullBounds $ [ ((s,t),sum [ a ! (s,u) * b ! (u,t) | u <- fullRange]) | (s,t) <- fullRange ]

power :: Matrix -> Int -> Matrix
power a n = case n `compare` 1 of
    EQ -> a
    GT -> let (m,r) = n `divMod` 2
              b     = power a m
          in  case r of
                0 -> b `multiply` b
                1 -> (b `multiply` b) `multiply` a
    LT -> error "power: exponent too small"

solution :: Int -> IntX8
solution n = let a = power edges (n-1)
             in  a ! (Outer ,Outer ) + a ! (Outer ,OuterX)

paths :: Cut -> Cut -> Int -> [[Cut]]
paths = memo3 enum enum bits paths'

paths' :: Cut -> Cut -> Int -> [[Cut]]
paths' s t 0 = if s == t then [[]] else []
paths' s t 1 = if edges ! (s,t) == 1 then [[t]] else []
paths' s t n = let m = n `div` 2
               in  [ p ++ q | u <- fullRange, p <- paths s u m, q <- paths u t (n-m) ]