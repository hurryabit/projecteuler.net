module Util where

import Control.Monad
import Data.List

chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr $ \xs -> guard (not (null xs)) >> return (splitAt n xs)

isSucc :: Enum a => a -> a -> Bool
isSucc e1 e2 = fromEnum e1 - fromEnum e2 == 1
