module Main where --Problem14 where

import Data.Array
import Data.List
import Data.Ord

collatzArray :: Array Integer Integer
collatzArray = listArray size (map f (range size))
  where size = (1,2000000)
        f 1 = 1
        f n
          | even n    = 1 + g (n `div` 2)
          | otherwise = 2 + g (n + 1 + n `div` 2)
        g n
          | n > snd size = f n
          | otherwise    = collatzArray ! n

sol = maximumBy (comparing snd) . take 1000000 . assocs $ collatzArray

main = print sol
