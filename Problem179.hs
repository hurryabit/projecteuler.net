module Problem179 where

import Data.Array.IArray
import Numbers

main = print solution

solution :: Int
solution =
  let nds = elems $ numDivisorsArray (10^7-1)
  in  length . filter (uncurry (==)) $ zip nds (tail nds)
