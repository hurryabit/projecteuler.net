{-# LANGUAGE TupleSections #-}
module Problem308Spec where

import Prelude hiding (init)
import Data.Array
import Data.List hiding (init)
import Data.Maybe

--  0  1  2  3  4  5  6  7  8  9
--  2  3  5  7 11 13 17 19 23 29

type Reg = Int

type Mem = Array Reg Int

data Rule = [Reg] :-> [Reg]
    deriving (Show)

program :: [Rule]
program =
    [ [3,5] :-> [6]
    , [2,6] :-> [0,1,5]
    , [1,6] :-> [7]
    , [0,7] :-> [8]
    , [1,4] :-> [9]
    , [9]   :-> [3,4]
    , [8]   :-> [2,7]
    , [7]   :-> [3,4]
    , [6]   :-> []
    , [5]   :-> [4]
    , [4]   :-> [5]
    , [0]   :-> [1,2]
    , [3]   :-> []
    , []    :-> [2,4]
    ]

init :: Mem
init = listArray (0,9) (1:repeat 0)

next :: Mem -> Rule
next mem = fromJust $ find (\(lhs :-> rhs) -> all (\reg -> mem ! reg > 0) lhs) program

step :: Mem -> Mem
step mem = let (lhs :-> rhs) = next mem
           in  accum (+) mem $ map (,-1) lhs ++ map (,1) rhs

dump :: Mem -> IO ()
dump mem = putStrLn $ unwords [show ks,show n,show (next mem)]
    where ks = elems mem
          n = foldl' (\m (p,k) -> m*p^k) 1 (zip [2,3,5,7,11,13,17,19,23,29] ks)

count = map fst $ filter (all (0==) . tail . elems . snd) $ zip [0..] (iterate step init)