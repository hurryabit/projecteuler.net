module TheCurse where

import Control.Monad
import Data.Char
import Data.List

rotations :: [a] -> [[a]]
rotations xs = zipWith const (map (flip (zipWith const) xs) $ iterate tail $ cycle xs) xs

solve = do
    let as = [3,5,1,5,4,2,3,4]
    bs <- rotations [3,5,4,3,1,2,2,3]
    cs <- rotations [3,1,5,2,4,3,1,2]
    ds <- rotations [3,2,3,5,4,3,1,4]
    guard $ all (12 ==) $ zipWith4 (\a b c d -> a+b+c+d) as bs cs ds
    return (as,bs,cs,ds)


caesar :: String -> Int -> String
caesar xs k = map (\x -> if x == ' ' then ' ' else chr $ ord 'a' + (ord x - ord 'a' + k) `mod` 26) xs

text = "wkh dqvzhu wr wkh ulggoh lv wkh zrug krxvh"