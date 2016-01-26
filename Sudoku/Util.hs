module Sudoku.Util
  ( multiInsert
  , isSingleton
  , accumM
  ) where

import Control.Monad
import Data.Array.IArray

multiInsert :: Int -> a -> [a] -> [a]
multiInsert = multiInsert' 0
  where
    multiInsert' :: Int -> Int -> a -> [a] -> [a]
    multiInsert' 0 n y xs     = y:multiInsert' n n y xs
    multiInsert' _ _ _ []     = []
    multiInsert' m n y (x:xs) = x:multiInsert' (m-1) n y xs

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleTon _   = False

accumM :: (Monad m, IArray a e, Ix i) =>
  (e -> e' -> m e) -> a i e -> [(i, e')] -> m (a i e)
accumM f a vs = do
  cs <- mapM (\(i,v) -> liftM ((,) i) (f (a ! i) v)) vs
  return $ a // cs
