module Problem306 where

import Data.MemoCombinators as Memo

type Position = [Int]

alter :: Int -> Position -> Position
alter n [] = [n]
alter n (m:ms) = case n `compare` m of
    LT -> n:m:ms
    EQ -> ms
    GT -> m:alter n ms

views :: Position -> [(Int,Position)]
views [n]    = [(n,[])]
views (n:ns) = (n,ns):[(m,n:ms) | (m,ms) <- views ns]

successors :: Position -> [Position]
successors pos = do
    (n,pos') <- views pos
    k <- [0..(n-2) `div` 2]
    return $ alter (n-2-k) . alter k $ pos'

forceWin :: Position -> Bool
forceWin = Memo.list Memo.integral forceWin'
    where forceWin' pos
            | all (<= 1) pos = False
            | otherwise      = any (not . forceWin) (successors pos)

a215721 :: [Int]
a215721 = 0:1:5:9:15:21:25:29:35:39:43:55:59:63:73:77:map (34+) (drop 11 a215721)
