module ListPlus
    ( module Control.Monad
    , module Data.List
    , merge
    )
where

import Control.Monad
import Data.List

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = case x `compare` y of
                        LT -> x:merge xs     (y:ys)
                        EQ -> x:merge xs     ys
                        GT -> y:merge (x:xs) ys
{-# SPECIALIZE merge :: [Int]     -> [Int]     -> [Int]     #-}
{-# SPECIALIZE merge :: [Integer] -> [Integer] -> [Integer] #-}