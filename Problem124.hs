module Problem124 where

import Data.Array
import Data.List
import Data.Ord

import Numbers

sol n = sortBy (comparing snd)
  [ (n,product (map fst fs)) | (n,fs) <- assocs (factorizationArray n) ]
