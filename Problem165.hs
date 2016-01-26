module Problem165 where

import Data.List
import Data.Maybe
import Data.Ratio

randS :: [Integer]
randS = tail $ iterate (\s -> (s*s) `mod` 50515093) 290797

randT :: [Integer]
randT = map (fromIntegral . flip mod 500) randS

type Point = (Rational,Rational)
type Segment = (Point,Point)

segments :: [Segment]
segments = take 5000 $ gen randT
    where gen (x1:y1:x2:y2:ts) = ((x1%1,y1%1),(x2%1,y2%1)):gen ts

intersection :: Segment -> Segment -> Maybe Point
intersection ((x1,y1),(x2,y2)) ((u1,v1),(u2,v2)) = case solve2 (x2-x1,y2-y1) (u1-u2,v1-v2) (u1-x1,v1-y1) of
    Nothing                                -> Nothing
    Just (s,t)
        | 0 < s && s < 1 && 0 < t && t < 1 -> Just (x1+s*(x2-x1),y1+s*(y2-y1))
        | otherwise                        -> Nothing

solve2 :: Point -> Point -> Point -> Maybe Point
solve2 (a,b) (c,d) (e,f)
    | det /= 0  = Just ((d*e-c*f) / det,(a*f-b*e) / det)
    | otherwise = Nothing
    where det = a*d - b*c

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

main = print . length . group . sort . catMaybes . map (uncurry intersection) . pairs $ segments