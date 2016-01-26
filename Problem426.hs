module Problem426 where

import Data.List

type BBS = [Int]

step :: BBS -> BBS
step [b1] = [b1]
step (b1:g1:b2:gbs)
    | b1 <= g1  = b1:(g1-b1+b2):step (b2:gbs)
    | otherwise = g1:b2        :step ((b2+b1-g1):gbs)

stable :: BBS -> Bool
stable [b1]           = True
stable (b1:g1:b2:gbs) = b1 <= g1 && b1 <= b2 && stable (b2:gbs)

state :: BBS -> [Int]
state [b1]       = [b1]
state (b1:_:bgs) = b1:state bgs

randS, randT :: [Int]
randS = 290797:map (\s -> s*s `mod` 50515093) randS
randT = map (\s -> s `mod` 64 + 1) randS