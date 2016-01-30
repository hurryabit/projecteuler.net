module Problem47 where

import Data.Array.IArray
import Data.List

import Factorizations

divisorCount :: Array Int Int
divisorCount = amap length factorizations

sols n = filter (all ((n <=) . snd)) . map (take n) . tails . drop 100000 .
  assocs $ divisorCount
