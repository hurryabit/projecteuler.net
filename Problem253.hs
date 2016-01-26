{-# LANGUAGE Rank2Types #-}
module Problem253 where

import Control.Arrow
import Data.List
import Data.MemoCombinators

type Gap = (Bool,Int,Bool)

start :: Gap
start = (False,40,False)

gap :: Memo Gap
gap = wrap (\(l,(n,r)) -> (l,n,r)) (\(l,n,r) -> (l,(n,r))) (pair bool (pair bits bool))

prob :: Int -> Gap -> Integer
prob = memo2 (arrayRange (1,19)) gap prob'

main = mapM_ (\s -> print $ prob s (False,10,False)) [1..5]

prob' s (l,n,r)
    | n > 0     =
        let ps = do
                    k <- [1..n]
                    let s' = s - fromEnum (not $ n == 1 || (k == 1 && l) || (k == n && r))
                    t <- [0 .. s']
                    return $ prob t (l,k-1,True) * prob (s'-t) (True,n-k,r)
        in  sum ps
    | s == 0    = 1
    | otherwise = 0